# fuss
(noun) : the quicker picker upper

An interactive tree utility for complete git workflows, written in modern Fortran.

## Features

- Shows a tree structure of git files (modified, staged, untracked)
- Proper UTF-8 tree rendering with box-drawing characters (`├──`, `└──`, `│`)
- **Color-coded status indicators:**
  - Green `↑` - Staged changes (ready to commit)
  - Yellow `↓` - pending changes from remote
  - Red `✗` - Modified tracked files
  - Dim grey `✗` - Untracked files
- Supports `--all`/`-a` flag to show all files (with status marked)
- Alphabetically sorted output matching the `tree` command format
- **Interactive mode** with full git workflow: stage, unstage, commit, push, diff (with pager), status (with pager), fetch, and pull

## Installing

### AUR

```bash
paru -S fuss
or
yay -s fuss
```

### Homebrew

```bash
brew tap FortranGoingOnForty/fuss
brew install fuss
```

### RPM
```bash
sudo dnf config-manager --add-repo https://repos.musicsian.com/musicsian.repo
sudo dnf install fuss
```

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
./fuss -a       # Shorthand
```

Interactive mode (full git workflow):
```bash
./fuss -i
./fuss --interactive
./fuss -i -a    # Interactive mode with all files
```

### Interactive Mode Controls

**Navigation:**
- `j` or `↓`: Move down
- `k` or `↑`: Move up

**Git Operations:**
- `a`: Stage file (git add)
- `u`: Unstage file (git restore --staged)
- `m`: Commit with message prompt
- `f`: fetch from remote
- `l`: pull from remote
- `d`: diff selected file in pager
- `p`: Push to remote
- `s`: View full git status (scrollable with `less`)

**Other:**
- `q`: Quit interactive mode
## Example Output

### Normal Mode

Dirty files only:
```
.
├── README.md ✗      # Red ✗ = modified
├── fuss.f90 ✗       # Red ✗ = modified
└── new_file.txt ✗   # Grey ✗ = untracked
```

All files:
```
.
├── .gitignore
├── Makefile
├── README.md ✗      # Red ✗ = modified
├── fuss ↑           # Green ↑ = staged
├── fuss.f90 ↑✗      # Both staged AND unstaged changes
└── fuss.o
```

### Interactive Mode

```
fuss:trunk           # Cyan repo name : Yellow branch name

.
├── README.md ✗      # ← Currently selected (highlighted)
├── fuss.f90 ↑
└── src
    └── main.f90 ✗

↑=staged ✗=modified ✗=untracked
j/k/↓/↑: navigate | a: stage | u: unstage | m: commit | p: push | s: status | q: quit
```

**Status Indicators:**
- Green `↑` - Staged (ready to commit)
- Red `✗` - Modified tracked file
- Dim grey `✗` - Untracked file
- `↑✗` - File has both staged and unstaged changes

### Interactive Mode Details

Interactive mode provides a complete git workflow TUI:

- Repository and branch name in status bar (`repo:branch`)
- Tree navigation through all files and directories
- Color-coded status indicators (staged, modified, untracked)
- Visual highlighting of selected item

**Git Workflow:**
- **Stage** (`a`) - Add files to staging area with `git add`
- **Unstage** (`u`) - Remove from staging with `git restore --staged`
- **Commit** (`m`) - Interactive commit message prompt
- **Push** (`p`) - Push commits to remote repository
- **Status** (`s`) - View full `git status` output in scrollable `less` viewer

**Technical Details:**
- Uses `stty cbreak -echo` for character-by-character input with proper newline handling
- ANSI escape codes for colors and highlighting (`ESC[7m` for selection, `ESC[31m` for red, etc.)
- Reads arrow key escape sequences (`ESC[A`, `ESC[B` for up/down)
- Automatically refreshes view after git operations
- Restores terminal with `stty sane` on exit

**Directory Expansion:**
- Automatically expands untracked directories (e.g., `dir/` → shows all files inside)
- Proper nested directory tree structure maintained

