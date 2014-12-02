retools
=======

Haskell RethinkDB API Tools I can't think of a name or good place for. 

Getting Started
---------------

[Get started with Haskell](https://github.com/bitemyapp/learnhaskell)

Test to see if this compiles:
  
    cabal sandbox init
    cabal install

Using in another project
------------------------

I haven't published this to hackage yet (still in flux)

First, import it into your project via git

    mkdir lib/
    git clone git@github.com:KualiCo/retools.git
    git add retools/

Then add it to cabal

    cabal sandbox add-source lib/retools

Then you can build your project normally
