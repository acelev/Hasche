require 'spec_helper'

describe Parser do

  it "should parse an IntVal" do
    Parser.parse("(3)").should == [3]
  end

  it "should parse an Ident" do
    Parser.parse("(x)").should == [:x]
  end

  it "should parse a Lambda" do
    Parser.parse("(lambda (x y) (+ 3 x y))").should == [:lambda, [:x, :y], [:+, 3, :x, :y]]
  end

  it "should parse a Define" do
    Parser.parse("(define (x y) (* 3 y))").should == [:define, [:x, :y], [:*, 3, :y]]
  end

  it "should parse a function call" do
    Parser.parse("(proc x y 3 z)").should == [:proc, :x, :y, 3, :z]
  end

end
