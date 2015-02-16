Homepage: [https://github.com/thomaseding/up](https://github.com/thomaseding/up)

Hackage: [https://hackage.haskell.org/package/up](https://hackage.haskell.org/package/up)



------


Install from git repository:
```
$ cabal configure
$ cabal build
$ cabal install
```

Install from Hackage.
```
$ cabal update
$ cabal install Fungi
```

These will install the `up` command.

--------

Example usage:
```
$ cd /home/thomas/code/up/src
$ up thomas
../../..
$ up --absolute thomas
/home/thomas
$ up ....
../../..
$ up --relative --from-to /abc/def/xyz/ abc
../../..
```

See `up --help` for more info.

------------

`up` generates file path strings, but it (by itself) does not change your current working directory. So below is code you can add to your shell's config file (e.g. `.bashrc`) to use the `up` program to change your directory.


```
g () {
    local DEST
    DEST=$(up "$@")
    if [ "$?" == '0' ]
    then
                cd "$DEST"
    else
                return "$?"
    fi
}
```

```
$ cd /home/thomas/code/up/src
$ g thomas
$ pwd
/home/thomas
```


For shell autocomplete for the above `g` command, you can add this to your shell's config in addition:
```
_g () {
        if [ "$COMP_CWORD" == "1" ]
        then
                local cur=${COMP_WORDS[COMP_CWORD]}
                local parts="$(pwd | tr '/' ' ')"
                COMPREPLY=( $(compgen -W "$parts" -- $cur) )
        fi
}
complete -F _g g
```


