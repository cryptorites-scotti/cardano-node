# tx-generator Service and Maintenance Manual
This document describes some key concepts of the tx-generator
and is a good starting point for anybody who wants to fix bugs
or add new features to the tx-generator.

## Benchmarking Scripts
The transaction generator is basically an interpreter for
benchmarking scripts.
A script defines how transactions are generated and transmitted.
Scripts can also query the state of the blockchain
to wait for era transitions
and it is also possible to write scripts that never interact
with Cardano-node, but instead write all the generated transactions to a file.
Benchmarking scripts (or just scripts) should not be confused with Plutus scripts.

## Main
The start-up of the tx-generator is pretty simple:

### `Cardano.Benchmarking.Command.runCommand`

* Call `withIOManager`.
* Parse the CLI arguments.
* Read and parse the benchmarking script.
* Call `Cardano.Benchmarking.Script.runScript`.
* Catch some errors.

In the executable `main = runCommand`.

### `Cardano.Benchmarking.Script.runScript`
* Setup the `ActionM` Monad.
* Initialize some rudimentary Tracing/Logging.
* Initialize the globalWallet.
* Interpret the benchmarking Script: `forM_ script action`
* Catch error and shutdown the Logging layer.

Note:
Many (but not all) scenarios of using the tx-generator involve additional setup actions.
For example: Reading the protocol parameters, reading a node-config,
starting a full cardano-node compatible logging infrastructure, etc.
These extra setup steps are provided by explicit script commands at the top
of the script.

Scripts that do not make use of node-to-node connections do not need
to run some of the setup step.
Examples are scripts for benchmarking the tx-generator itself,
or smoke tests, that write the generated txs to a file instead of transmitting
them to a node.

### `Cardano.Benchmarking.Script.Action.action :: Action -> ActionM ()`
This is a big case-dispatch which calls the implementation of each action.

### `module Cardano.Benchmarking.Script.Core`
This module is a collection of reusable building blocks for benchmarking scripts
and the implementations for all the script actions.
Most function here run in the `ActionM` monad.

## Benchmarking Scripts , State and the  `ActionM` Monad

Scripts send transactions to the blockchain and spend UTxOs.
Therefore they are inherently stateful.
The tx-generator maintains internal state (environment)
and tries to keep track of the set of UTxOs that are available for the tx-generator.

The internal state of the tx-generator is available via the `ActionM` Monad.
This includes `MVar`s which contain IO-update-able state.
The state/environment is defined in `Cardano.Benchmarking.Script.Env`.

## Wallets and Funds
The transaction generator has an internal notion of wallets and funds.
Wallets store a set of Funds.
A Fund is basically an UTxO together with
all the information that is needed for spending,
i.e. keys, Plutus scripts and Plutus redeemers, etc.

Note:
The wallet implementation uses `ixset-typed.
Unfortunately, it turned out that
queries have a poor complexity in `ixset-txped`.
More precisely, a query seems to be linear in the size of data set.
(In practice, it becomes notable at about 10_000 stored funds
and takes minutes for data sets of size  1_000_000.)
The computational costs is the same regardless
whether one extracts one fund from the query or all the matching funds.

The tx-generator uses the following workaround to speed up the queries.
Instead of running an `ixset-typed` query for each fund/UTxO,
it queries all the funds that are needed (for a particular phase)
in on query and buffers the funds in a plain old list.

This workaround works well in the current state of the tx-generator.
`ixset` based wallets provide a nice abstraction for future
extension to the tx-generator, but one has to keep in mind
the complexities of the data structures.

## Lifetime of a Transaction
Benchmarking scripts can create a variety of transactions for different purposes:

* Splitting funds.
* Regular transactions.
* Paying to a Plutus script.
* Spending a script address.

Under the hood, all transactions are generated in the same function
and with the same workflow.
This is done in `Cardano.Benchmarking.Wallet.sourceToStoreTransaction`.
It performs the following steps:

* Select the input Funds:
  `type FundSource = IO (Either String [Fund])`
* Determin output values (value preserving) :
  `[Lovelace] -> [Lovelace]`
* Create UTxOs for the ouput values:
  `type ToUTxO era = [Lovelace] -> ([TxOut CtxTx era], TxId -> [Fund])`
* Create the actual transaction:
  `type TxGenerator era = [Fund] -> [TxOut CtxTx era] -> Either String (Tx era, TxId)`
* Put the new funds in a wallet:
  `type FundToStore = [Fund] -> IO ()`

The five arguments of `sourceToStoreTransaction` are
the callback functions for the five steps in the list above.
`FundSource` removes Funds from the wallet
and `FundToStore` adds new Funds, and those two functions are `IO` actions.
When sending transaction to multiple nodes,
each node-to-node submission protocol runs in a separate thread
and each thread calls `sourceToStoreTransaction` independently.

## Streaming abstractions

## Gotchas
There are many !

## JSON Script Language
The script language is defined in `module Cardano.Benchmarking.Script.Types`.
The script language is highly imperative.
A benchmarking script consists of a list of statements.
Statements can generate or transmit transactions,
set or update global variables, spawn new treads or perform other IO actions.
A statemtent that accesses an unset global variable throws a runtime exceptions.
A gotcha of the script language that not all syntactical possible statements are
actually implemented.
Serveral statements contain an `SubmitMode`
```
data SubmitMode where
  LocalSocket :: SubmitMode
  NodeToNode  :: SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  deriving (Show, Eq)
deriving instance Generic SubmitMode
```
but do not implement all the possible `SubmitMode`s.

## Nix and the tx-generator-service definition
The benchmarks on AWS calls the tx-generator from a dedicated systemd service.
The nix module in `nix/nixos/tx-generator-service.nix` defines the possible
module options that are available to configure the tx-generator service
and also the logic to generate the JSON benchmarking script based on these modules.

The benchmarking profiles for the AWS cluster define the tx-generator service
in terms of the nix module options and `nix/nixos/tx-generator-service.nix` translates these
to the actual JSON benchmarking script.
When the tx-generator script language is changes `nix/nixos/tx-generator-service.nix`
may need to be updated as well.

## Benchmarking scripts.
The Haskell data type `Action` defines the available commands/actions of the script language:
```
data Action where
  Set                :: !SetKeyVal -> Action
  StartProtocol      :: !FilePath -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  DefineSigningKey   :: !KeyName -> !TextEnvelope -> Action
  AddFund            :: !TxIn -> !Lovelace -> !KeyName -> Action
  ImportGenesisFund  :: !SubmitMode -> !KeyName -> !KeyName -> Action
  CreateChange       :: !SubmitMode -> !PayMode -> !Lovelace -> !Int -> Action
  RunBenchmark       :: !SubmitMode -> !SpendMode -> !ThreadName -> !NumberOfTxs -> !TPSRate -> Action
  WaitBenchmark      :: !ThreadName -> Action
  CancelBenchmark    :: !ThreadName -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyCardanoEra -> Action
  SetProtocolParameters :: ProtocolParametersSource -> Action
  deriving (Show, Eq)
deriving instance Generic Action
```

Here is an example of a complete benchmarking script in JSON syntax:

```
[{"Set":{"SNumberOfInputsPerTx":2}},
 {"Set":{"SNumberOfOutputsPerTx":2}},
 {"Set":{"SNumberOfTxs":1000}},
 {"Set":{"STxAdditionalSize":100}},
 {"Set":{"SMinValuePerUTxO":10000000}},
 {"Set":{"SFee":1000000}},
 {"Set":{"STTL":1000000}},
 {"StartProtocol":"/nix/store/xkp61kk45haanxyc3cj8vy4pa7cywpp0-generator-config.json"},
 {"Set":{"SEra":"Alonzo"}},
 {"Set":{"STargets":[{"addr":"54.79.76.202","port":3001},{"addr":"18.233.192.149","port":3001}]}},
 {"Set":{"SLocalSocket":"/var/lib/cardano-node/node.socket"}},
 {"ReadSigningKey":["pass-partout","/var/lib/keys/cardano-node-signing"]},
 {"ImportGenesisFund":[{"LocalSocket":[]},"pass-partout","pass-partout"]},
 {"Delay":90},
 {"CreateChange":[{"LocalSocket":[]}, {"PayToAddr":"pass-partout"},9481000900,3]},
 {"Delay":90},
 {"CreateChange":[{"LocalSocket":[]}, {"PayToAddr":"pass-partout"},316000030,67]},
 {"Delay":90},
 {"CreateChange":[{"LocalSocket":[]}, {"PayToAddr":"pass-partout"},10500001,2000]},
 {"Delay":90},
]
```
The JSON syntax uses the generically derived `Aeson` instance of `Action`.
In other words: There is a one-to-one correspondence between the Haskell representation of the scripts
and the JSON format and this documentation will use the Haskell side whenever possible because it is easier
to read.

### Setup configuation options
The command `Set :: !SetKeyVal -> Action` is used to set the value of some global and mutable variables/
configuration options. for example `Set (SNumberOfInputsPerTx 2)`
Typical options are:

* `SNumberOfOutputsPerTx 2' : Use 2 number of outputs per transaction

* `STxAdditionalSize 100` : Add 100 additional dummy Bytes to include in every transaction

* `SMinValuePerUTxO 10000000` : The minimum value of an UTxO. This is the value used in the transaction generator.
  This value must not be smaller than the corresponding protocol parameter.

* `SFee 1000000' : Set a fixed fee used in the transaction generator.

* `SEra Alonzo` : This sets the target format for the generated transactions.

* `{"STargets":[{"addr":"54.79.76.202","port":3001},{"addr":"18.233.192.149","port":3001}]}'
  These are the target nodes for node-to-node submissions.
  For a 50-node AWS cluster this would be a list of 50 addresses/ports.

* 'SLocalSocket "/var/lib/cardano-node/node.socket' : Set the target for the local submission protocol.

The command `StartProtocol :: !FilePath -> Action'
reads the protocol definitions from a file, and starts up the logging layer.

'ReadSigningKey :: !KeyName -> !SigningKeyFile -> Action'
reads a signing key from a file and stores it in a named key-variable.
'DefineSigningKey :: !KeyName -> !TextEnvelope -> Action` does the same for an implicitly defined key.


### Import available genesis funds
After setup of the global options, the script will typically import a number of initial funds.
'ImportGenesisFund  :: !SubmitMode -> !KeyName -> !KeyName -> Action'
The first `KeyName` is the key for spending the externally defined fund, the second `KeyName` give the target address.

### Tree based splitting
Typically benchmarks runs consist of two phases: An initial UTxO splitting phase and the actual benchmarking phase.
The purpose of the splitting phase is to create the UTxOs that are needed in the actual benchmarking phase.
'''
 {"CreateChange":[{"LocalSocket":[]}, {"PayToAddr":"pass-partout"},9481000900,3]},
 {"Delay":90},
 {"CreateChange":[{"LocalSocket":[]}, {"PayToAddr":"pass-partout"},316000030,67]},
 {"Delay":90},
 {"CreateChange":[{"LocalSocket":[]}, {"PayToAddr":"pass-partout"},10500001,2000]},
 {"Delay":90},
'''

Each call of 'CreateChange :: !SubmitMode -> !PayMode -> !Lovelace -> !Int -> Action'
creates 'n' UTxOs of a certain value.
`CreateChange` can create normal UTxOs ('PayToAddr :: !KeyName -> PayMode') or
pay to Scripts ('PayToScript :: !FilePath -> !ScriptData -> PayMode').
It can also create special outputs that are set a side as collaterals
and different `SubmitMode`s are possible.

Note: Internally, `CreateChange` is hardcoded to generate transaction with one input
and up to 30 outputs of the desired value plus possibly one output with change.

The example above does the follwing:
* Create 3 outputs of 9481000900 Lovelaces (in one transaction)
* Delay 90 seconds
* Create 67 outputs of 316000030 Lovalaces (uses three transactions)
* Delay 90 seconds
* Create 2000 outputs of 10500001 Lovalaces (uses 67 transactions)

In this example each preceding call of `CreateChange` ensures that
the necessary number of UTxOs of high-enough value are available
for the subsequent call of `CreateChange`.

Note: The UTxOs are NOT directly passed from on call of `CreateChange` to the next.
Each `CreateChange` consumes funds from the global wallet and returns its outputs
back to that wallet.
More precisely, `CreateChange` merely selects UTxOs of the the needed value in the wallet.
These inputs may have been created by the preceding `CreateChange` or come from somewhere else.

The described scheme creates a tree of transactions where the lower layer spends the outputs that
have been created on the layer(s) above.

### Lost transactions forks and rollbacks.

Note: An earlier versions of the tx-generator had used a linear splitting scheme.
In the linear scheme, each transaction generates a fixed number of
outputs plus one dedicated change output and that change is used as input for the next transactions.

The problem is that transactions may get lost due to rollbacks on the chain and
in the linear scheme any lost transaction will breakup the chain

With a  tree based splitting scheme a
lost transaction will (most likely) only affect a single leave. (The trees have a branching degree of 30).
For example, a real world benchmarking runs may use 50_000 transactions (or even more).
With tree based splitting, 50_000 output UTxOs are created with a tree of hight four.
The inner layers of the splitting tree have 1, 2 and 56 transactions
while 1667 of transactions are leaves of the transaction tree.

A normal blockchain application will wait for transaction confirmations to get the desired
level of assurance that a transaction is final and retry a transaction if it was not included in the chain.
It may be possible to improve the tx-generator with some kind of confirmation queries,
but the most important lesson from tx-generator hacking is to avoid linear chains of transactions.

### Submit Modes
Submit Modes targets are defined in
```
module Cardano.Benchmarking.Script.Types
data SubmitMode where
  LocalSocket :: SubmitMode
  NodeToNode  :: SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  deriving (Show, Eq)
deriving instance Generic SubmitMode
```

Not all commands that take a `SubmitMode` argument support all submit modes.

### Benchmarking Phase
Note: The command `RunBenchmark :: !SubmitMode -> !SpendMode -> !ThreadName -> !NumberOfTxs -> !TPSRate -> Action`
basically branches into several independent implementations depending on the arguments for `SubmitMode` and `SpendMode`.
`NodeToNode  :: SubmitMode' is used for regular benchmarking.
The `SubmitMode`s `LocalSocket`, `DumpToFile` and `DiscardTX` are used for debugging and benchmarking the tx-genertor itself
and behave sightly different than the node-to-node mode.
This section descripts the `NodeToNode` case.

In the `NodeToNode` case, `RunBenchmark` starts a new benchmarking thread and transmits a total count of `NumberOfTXs`
at a given `TPSRate`.
Under the hood, it starts a new node-to-node submission client for each remote node.
The `TPSRate` is the maximum overall transaction rate that is offered to the nodes.
Each node can request transactions at its own pace but the tx-generator will not generate transcations faster
than `TPSRate`.

After a benchmarking thread has been started, the script interpreter immediately procceeds to the next command in the benchmarking script.
Note; If `RunBenchmark` is the last command in the benchmarking script, the script immediately terminates without running the benchmark properly.
In the simple case, `RunBenchmark` is followed by a  `WaitBenchmark :: !ThreadName -> Action`, which blocks until the benchmarking
thread with a given name has terminated.
A benchmarking script that is supposed to run for a fixed time span (for example 10 hours) can be implemented with the following pattern:
```
RunBenchmark ...
Delay 36000
CancelBenchmark ...
```

### Heterogeneous work loads and complex benchmarking scenarios.
The command `RunBenchmark`, `Delay`, `CancelBenchmark` and `WaitBenchmark` can be combined in an intuitive way
to build more complex workloads, or workload that change over time.
For example:
Stress the cluster with a heterogeneous work load:
```
RunBenchmark ... (ThreadName "normal") ...          # start some basic workload
RunBenchmark ... (ThreadName "plutus-scripts") ...  # start some basic workload
Delay 36000
CancelBenchmark (ThreadName "normal" )
CancelBenchmark (ThreadName "plutus-scripts")
```

Test how the cluster recovers from a high workload:
```
RunBenchmark ... (ThreadName "overload") ...  # overload the cluster
Delay 3600  # for one hour
CancelBenchmark (ThreadName "overload" )

RunBenchmark ... (ThreadName "cool-down") ...  # small workload during a cool-down period
Delay 18000  # for five hours
CancelBenchmark (ThreadName "cool-down" )
```

The command `WaitForEra :: !AnyCardanoEra -> Action` queries the current era of the cluster and
blocks until the cluster reports the given target era.
This can be used as follows:
```
Set (SEra Mary) # run the tx-generator in Mary mode.
RunBenchmark ... (ThreadName "mary-workload") ...
WaitForEra Alonzo
CancelBenchmark (ThreadName "mary-workload" )

Set (SEra Alonzo) # switch the tx-generator to Alonzo mode.
RunBenchmark ... (ThreadName "alonzo") ...
Delay 3600
CancelBenchmark (ThreadName "alonzo" )

### Debuging `RunBenchmark`
The `SubmitMode`s `LocalSocket`, `DumpToFile` and `DiscardTX` change the semantics of `RunBenchmark`.
In these modes `RunBenchmark` generates one transcation after the other, as fast as possible.
It submits the transactions to a local socket, writes them to a file or discards them.
In these mode `RunBenchmark` does not for a new thread and blocks until `NumberOfTXs` have been processed.
`RunBenchmark` ignores the thread name argument and a subsequent
`WaitBenchmark` will fail with a lookup-error.


### Plutus Benchmarks
Plutus benchmarks are defined via `PayMode` and `SpendMode`

```
data PayMode where
  PayToAddr :: !KeyName -> PayMode
  PayToCollateral :: !KeyName  -> PayMode
  PayToScript :: !FilePath -> !ScriptData -> PayMode

data SpendMode where
  SpendOutput :: SpendMode
  SpendScript :: !FilePath -> ScriptBudget -> !ScriptData -> !ScriptRedeemer -> SpendMode
  SpendAutoScript :: !FilePath -> SpendMode
```

PayMode is used in `CreateChange` to create UTxOs while `SpendMode` is used in `RunBenchmark`.
`PayToCollateral` is used to set aside an UTxO that is used as collateral.

## NodeToNode Submission
