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