Hasche
======

Scheme interpreters in Python, Ruby, and Haskell

# Ruby Version

First make sure you have ruby 1.9.3 and the bundler installed.

To install bundler and the associated gems (from Hasche/sche): 

` $ sudo gem install bundler`

` $ bundle install`

To run the interpreter in interactive mode:

` $ ruby sche.rb -i`

>/~(8)~\

Alternatively, you can pass a Scheme expression to sche.rb as a command line argument:

` $ ruby sche.rb "(list 1 2 3 4)"`
> => (1 2 3 4)

There is a minimal test suite included. If you want to run the tests (from Hasche/sche):

` $ rspec spec`