retools
=======

Haskell RethinkDB API Tools I can't think of a name or good place for. 

Getting Started
---------------

[Get started with Haskell](https://github.com/bitemyapp/learnhaskell)

To see if retools compiles
  
    cabal sandbox init
    cabal install

Using in another project
------------------------

I haven't published this to hackage yet (still in flux)

First, import it into your project via git

    mkdir lib/
    git clone git@github.com:KualiCo/retools.git
    git add retools/

Then add the source to cabal

    cabal sandbox add-source lib/retools

And add `retools` to your .cabal file

    build-depends:       base >=4.7 && <4.8,
                         retools,
                         ...

Then you can import into your project and build normally

    import Database.RethinkDB.ReTools
