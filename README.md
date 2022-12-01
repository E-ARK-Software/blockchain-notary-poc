# Installation

## Prerequesites

* GHC 8.10.x
* Cabal
* Go Ethereum 1.10.x

## Quick Instructions for Running a Single Node Ethereum Private Network

1. Install Go Ethereum (https://geth.ethereum.org/downloads/)

2. create an account

       geth account new

3. create a genesis block with `puppeth` with the configuration:
    - clique (Proof-of-Authority)
    - prefunding the account created in the previous step and setting it as sealer

4. initialise chain with genesis block

       geth init genesis.json

5. create password file for the account

6. start node
    
       geth --allow-insecure-unlock --http --nodiscover --unlock $address --password $password_file --mine


## Installation

1. install GHC 8.10.x and Cabal (https://www.haskell.org/downloads/)

2. deploy the smart contract

       geth js timestamp/deploy_web3_0.20.js

3. add the address of the smart contract in `config.ini`

4. install dependencies and run web server

       cabal run

## Run Queries

write data

    $ object_id=62962ee9-4331-4eb0-b6ea-0ac1236d4219
    $ license_hash=e0fa51e632e434279359d288b851fb7b6a7de56d3106d5ece6d52de4d73f4018
    $ requester=a8c898c0e5f2f7f4817ca39b0aa37bc5e6194442

    $ curl -X POST http://localhost:8081/registerObject?objectId=$object_id
    {
      "message":"The information has been registered successfully under the hash 62962ee943314eb0b6ea0ac1236d421900000000000000000000000000000000."
    }

    $ curl -X POST http://localhost:8081/createLicense?licenseHash=$license_hash
    {
      "message":"The information has been registered successfully under the hash e0fa51e632e434279359d288b851fb7b6a7de56d3106d5ece6d52de4d73f4018."
    }

    $ curl -X POST "http://localhost:8081/assignLicense?objectId=$object_id&licenseHash=$license_hash"
    {
      "message":"The information has been registered successfully under the hash bca16b2ea551dacf5c0eb94416379b7344883ad78a408d6a173c24f526977cc3."
    }

    $ curl -X POST "http://localhost:8081/acceptLicense?requester=$requester&objectId=$object_id&licenseHash=$license_hash"
    {
      "message":"The information has been registered successfully under the hash 95a51b0e554cfbfa8457429b3310c5c6270c2cad58359a667758c09e798768cb."
    }
    
    $ curl -X POST "http://localhost:8081/approveRequest?requester=$requester&objectId=$object_id"
    {
      "message":"The information has been registered successfully under the hash d131caea0553bf55dd2af1481d9f66d3522fed2f12cc835fd5e50b9805051f2e."
    }

query timestamps

    $ curl http://localhost:8081/timestamp?hash=d131caea0553bf55dd2af1481d9f66d3522fed2f12cc835fd5e50b9805051f2e
    {
      "hash":"d131caea0553bf55dd2af1481d9f66d3522fed2f12cc835fd5e50b9805051f2e",
      "timestamp":"1669377213",
      "creator":"0x0f67f65c5b48786921af60639b370e906e829672"
    }
