Name:           fuss
Version:        0.9.96
Release:        1%{?dist}
Summary:        A tree utility for dirty git files, written in modern Fortran

License:        MIT
%global debug_package %{nil}
URL:            https://github.com/FortranGoingOnForty/fuss
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  gfortran >= 4.8
BuildRequires:  make
Requires:       glibc
Requires:       git

%description
FUSS is a tree utility for dirty git files, written in modern Fortran.
It displays a tree structure of dirty git files (modified, untracked, etc.)
with proper UTF-8 tree rendering using box-drawing characters.

Features:
- Shows a tree structure of dirty git files (modified, untracked, etc.)
- Proper UTF-8 tree rendering with box-drawing characters (├──, └──, │)
- Marks dirty files with ✗
- Supports --all flag to show all files (with dirty files marked)
- Alphabetically sorted output matching the tree command format

%prep
%autosetup

%build
make

%install
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_docdir}/%{name}

# Install binary
install -Dm755 fuss %{buildroot}%{_bindir}/fuss

# Install documentation
install -Dm644 README.md %{buildroot}%{_docdir}/%{name}/README.md

%files
%{_bindir}/fuss
%{_docdir}/%{name}/README.md

%changelog
* Tue Nov 05 2024 mfw <espadon@outlook.com> - 0.9.96-1
- Enhancements and fixes

* Tue Nov 05 2024 mfw <espadon@outlook.com> - 0.9.94-1
- Enhancements and fixes

* Tue Nov 05 2024 mfw <espadon@outlook.com> - 0.9.92-1
- Enhancements and fixes

* Tue Nov 05 2024 mfw <espadon@outlook.com> - 0.9.9-1
- Enhancements and fixes

* Mon Nov 04 2024 mfw <espadon@outlook.com> - 0.9.8-1
- Enhancements and fixes

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.9.6-1
- Enhancements and fixes

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.9.5-1
- Minor annoyance fixed

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.9.4-1
- Bug fixes and improvements

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.9.3-1
- Continued improvements and fixes
- Enhanced stability and performance

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.9.2-1
- Bug fixes and enhancements
- Additional git operations improvements

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.9.0-1
- Major feature update with enhanced git operations
- Improved tree navigation and functionality

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.7.1-1
- Add expand/collapse directory functionality with arrow keys
- Enhanced tree navigation and user interaction

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.7.0-1
- Minor version bump with additional git operations
- Enhanced functionality and improvements

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.6.5-1
- Add tag functionality with 't' key
- Enhanced git operations with tagging support

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.6.4-1
- Fix compiler line length limits for all build environments
- Ensures -ffree-line-length-none is included in Makefile

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.6.3-1
- Reduced frequency of upstream prompts
- Improved upstream configuration handling

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.6.2-1
- Fetch operations now refresh tree and maintain indicators
- Updated README documentation

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.6.1-1
- Fix Fortran line length limits with -ffree-line-length-none flag
- Ensures compatibility across different gfortran configurations

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.6.0-1
- New git operations suite
- Enhanced git functionality and workflow improvements

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.5.2-1
- Added .NOTPARALLEL to Makefile for Homebrew compatibility
- Ensures modules are built sequentially to avoid parallel build issues

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.5.1-1
- Fixed Makefile module dependencies for parallel builds
- Resolved compilation errors with module file dependencies

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.5.0-1
- Latest improvements and enhancements

* Sat Oct 19 2025 mfw <espadon@outlook.com> - 0.4.0-1
- Full git operations suite implementation
- Enhanced git integration and functionality

* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.3.4-1
- Code refactoring and improvements

* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.3.3-1
- Latest improvements and bug fixes

* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.3.2-1
- Final fix: Directory expansion now working correctly

* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.3.1-1
- Bug fix: Fixed dirty dirs not descending properly

* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.3.0-1
- Enhanced status indicators
- Shorthand for --all flag
- Color indicators for file status
- Mixed indicators matching git behavior

* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.2.0-1
- Interactive mode implementation
- Complete interactivity with navigation and quick staging
- Enhanced user experience for dirty file management

* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.1.0-1
- Initial release of FUSS
- Tree utility for dirty git files
- UTF-8 tree rendering with box-drawing characters
- Support for --all flag to show all files
- Alphabetically sorted output
