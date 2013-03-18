require 'treetop'
require_relative './node_extensions.rb'

class Parser

  Treetop.load("./parser.treetop")
  @@parser = SchemeParser.new

  def self.clean_tree(tree)
    return if tree.elements.nil?
    tree.elements.delete_if { |n| n.class == Treetop::Runtime::SyntaxNode }
    tree.elements.each      { |e| self.clean_tree(e) }
  end

  def self.parse(data)
   tree =  @@parser.parse(data)
    if tree.nil?
      puts @@parser.failure_reason
      raise Exception, "Parse error at offset: #{@@parser.index}"
    end
    self.clean_tree(tree)
    return tree.to_lisp
  end
end
