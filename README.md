damm
=====

[![Build Status](https://travis-ci.org/mururu/damm.svg?branch=master)](https://travis-ci.org/mururu/damm)
[![hex.pm version](https://img.shields.io/hexpm/v/damm.svg)](https://hex.pm/packages/damm)


An Erlang implementaion of the Damm algorithm created by H. Michael Damm.  
The Damm algoritm is a check digit algorithm which can detects all single-digit errors and all adjacent transposition errors.  
More details: [Damm algorithm - Wikipedia](https://en.wikipedia.org/wiki/Damm_algorithm)

## Usage

##### `damm:checksum/1`

```
1> damm:checksum(572).
4
2> damm:checksum("572").
4
3> damm:checksum(<<"572">>).
4
```

##### `damm:encode/1`

```
1> damm:encode(572).
5724
2> damm:encode("572").
"5724"
3> damm:encode(<<"572">>).
<<"5724">>
```

##### `damm:is_valid/1`

```
1> damm:is_valid(5724).
true
2> damm:is_valid("5724").
true
3> damm:is_valid(<<"5724">>).
true
4> damm:is_valid(5723).
false
```
