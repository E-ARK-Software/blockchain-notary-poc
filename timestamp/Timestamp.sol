// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.17;
contract Timestamp {

    struct TimestampInfo {
        uint timestamp;
        address creator;
    }

    mapping(bytes32 => TimestampInfo) public timestamps;
    
    function addValue(bytes32 data) public {
        timestamps[data] = TimestampInfo(block.timestamp, msg.sender);
    }

}
