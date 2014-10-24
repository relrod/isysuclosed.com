%define gitdate 20141024
Name:           isysuclosed
Version:        0.1.0.0
Release:        1.%{gitdate}git%{?dist}
Summary:        The "isysuclosed.com" webapp.
License:        BSD
URL:            https://github.com/relrod/isysuclosed.com
BuildRequires:  git ghc systemd

# This is disabled for my local builds, since I use cabal from git.
#BuildRequires: cabal-install

%description
Haskell (Snap Framework) app for isysuclosed.com

%prep
if [ -d isysuclosed.com ]; then
  cd isysuclosed.com
  git reset --hard && git pull
else
  git clone git://github.com/relrod/isysuclosed.com/
  cd isysuclosed.com
fi

%build
export LANG=en_US.UTF-8
cd isysuclosed.com
cabal sandbox init
cabal install --only-dependencies
cabal install

%check

%install
mkdir -p %{buildroot}/%{_bindir}
cd isysuclosed.com
cp .cabal-sandbox/bin/%{name} %{buildroot}/%{_bindir}/%{name}

# API conf (not stored in repo)
mkdir -p %{buildroot}/%{_sysconfdir}/%{name}
touch %{buildroot}/%{_sysconfdir}/%{name}/wunderground_api_key

# systemd
mkdir -p %{buildroot}/%{_unitdir}
cp pkg/%{name}.service %{buildroot}/%{_unitdir}/%{name}.service

%files
#%config(noreplace) %{_sysconfdir}/httpd/%{name}.conf
%config(noreplace) %{_sysconfdir}/%{name}/wunderground_api_key
%{_unitdir}/%{name}.service
%{_bindir}/%{name}

%changelog
* Fri Oct 24 2014 Ricky Elrod <rbelrod@student.ysu.edu> - 0.1.0.0-1.20141024git
- Initial build
