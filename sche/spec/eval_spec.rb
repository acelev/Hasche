require 'spec_helper'

def doit(program)
  evalSch(Parser.parse(program), $global_env)
end

describe "The Evaluator" do

  it "should evaluate arithmetic expressions" do
    doit("(+ 3 (* (+ 2 3) (/ 8 2)))").should == 23
  end

end

describe "Define" do

  it "should return the name of the defined function/variable" do
    doit("(define (fact n) " + 
           "(if (eq? n 0) " + 
               "1 " +
               "(* n (fact (- n 1))) ))"
         ).should == "fact"
  end

  it "should let me use the functions I define" do
    doit("(fact 5)").should == 120
  end

  it "should define variables, too" do
    doit("(define x 5)").should == 5
  end

  it "should support recursive higher order functions" do
    doit("(define (fold f init xs) " +
           "(if (eq? xs (quote ())) " +
                "init " +
                "(f (car xs) (fold f init (cdr xs))) ))").should == "fold"
  end

  it "should produce a working implementation of fold" do
    doit("(define (add x y) (+ x y))")
    doit("(fold add 0 (quote (1 2 3 4 5)))").should == 15
  end

  it "should support lambda" do
    doit("((lambda (x y) " +
                   "(fold x y '(1 2 3 4 5))) + 0)").should == 15
  end

end



