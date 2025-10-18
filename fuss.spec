Name:           fuss
Version:        0.1.0
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
* Sat Oct 18 2025 mfw <espadon@outlook.com> - 0.1.0-1
- Initial release of FUSS
- Tree utility for dirty git files
- UTF-8 tree rendering with box-drawing characters
- Support for --all flag to show all files
- Alphabetically sorted output
