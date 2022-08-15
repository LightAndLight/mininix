# Notes

## Test Cases

`builder.sh`:
```
#! /usr/bin/env bash

echo "hello"
```

`test.nix`:
```
action {
  inputs = {},
  builder = file "./builder.sh"
}
```

Rename `builder.sh` to `hello.sh` and update `test.nix`.

Rebuilding after renaming should reuse cached results. The action's key
should depend on `builder.sh`'s content, not its name, so 

## Scheduling

To build a dependency, I can build all the dependency's inputs in parallel.

```
 D   E   F
 ^   ^   ^
  \ / \ /
   B   C
   ^   ^
    \ /
     A
```

I must build A.

I put B and C onto the queue. All A's dependencies can be built concurrently.

   I must build B.
   
   I must build C.

Another take: execute in parallel every item whose dependencies have been satisfied.