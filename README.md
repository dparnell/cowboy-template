Cowboy Template
===============

This document descibes the basics of how to get the template up and running on a new machine

Installation
------------

Under Linux:

Install erlang and rebar with one of:

```
sudo yum install erlang rebar
sudo apt-get install erlang rebar
sudo pacman -Sy erlang rebar
```

On the Mac:

Install Erlang R16 and rebar using homebrew or any other method.

```
brew install erlang
brew install rebar
```

Compilation
-----------

To compile this you need Erlang and rebar in your PATH.  Under Windows you will also need to have Visual Studio installed (NOTE: only tested using Visual Studio 2010)

Type the following command:

```
make
```

or on Windows, start a cygwin prompt and type:

```
make
```

Running the template
--------------------

You can start the Erlang node with the following command:
```
./start.sh
```

Then point your browser to the indicated URL.

To exit use the following command:

```
q().
```
