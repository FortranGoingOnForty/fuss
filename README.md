# fuss

A tree utility for dirty git files, written in modern Fortran.

## Features

- Shows a tree structure of dirty git files (modified, untracked, etc.)
- Proper UTF-8 tree rendering with box-drawing characters (`├──`, `└──`, `│`)
- Marks dirty files with `✗`
- Supports `--all` flag to show all files (with dirty files marked)
- Alphabetically sorted output matching the `tree` command format

## Building

```bash
make
```

## Usage

Show only dirty files (default):
```bash
./fuss
```

Show all files with dirty ones marked:
```bash
./fuss --all
```
## Example Output

Dirty files only:
```
.
├── README.md ✗
├── fuss ✗
└── fuss.f90 ✗
```

All files:
```
.
├── .gitignore
├── Makefile
├── README.md ✗
├── fuss ✗
├── fuss.f90 ✗
└── fuss.o
```

Files marked with `✗` are dirty (modified or untracked).
