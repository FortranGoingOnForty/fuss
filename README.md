# fuss

An interactive tree utility for quickly staging dirty git files, written in modern Fortran.

## Features

- Shows a tree structure of dirty git files (modified, untracked, etc.)
- Proper UTF-8 tree rendering with box-drawing characters (`├──`, `└──`, `│`)
- Marks dirty files with `✗`
- Supports `--all` flag to show all files (with dirty files marked)
- Alphabetically sorted output matching the `tree` command format
- **Interactive mode** (`-i`/`--interactive`) with keyboard navigation and instant git add

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

Interactive mode (navigate and git add files):
```bash
./fuss -i
./fuss --interactive
./fuss -i --all    # Interactive mode with all files
```

### Interactive Mode Controls

- `j` or `↓`: Move down
- `k` or `↑`: Move up
- `Enter`: Git add the selected dirty file
- `q`: Quit interactive mode
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

### Interactive Mode Details

Interactive mode provides a TUI (Text User Interface) for staging files:
- Uses `stty raw -echo` to enable raw terminal input
- ANSI escape codes (`ESC[7m` / `ESC[0m`) for reverse video highlighting
- Reads arrow key escape sequences (`ESC[A`, `ESC[B`)
- Executes `git add` commands and refreshes the view automatically
- Restores terminal with `stty sane` on exit
