
module SchemeNodes

  class IntLit < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.text_value.to_i
    end
  end

  class StrLit < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.text_value
    end
  end

  class Ident  < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.text_value.to_sym
    end
  end

  class BoolVal < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.text_value == '#t'
    end
  end

  class Expr < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.elements[0].to_lisp
    end
  end

  class Body < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.elements.map { |e| e.to_lisp }
    end
  end

  class Keyword < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.text_value.to_sym
    end
  end

  class Operator < Treetop::Runtime::SyntaxNode
    def to_lisp
      self.text_value.to_sym
    end
  end

  class Quote < Treetop::Runtime::SyntaxNode
    def to_lisp
      :quote
    end
  end

  class FancyQuote < Treetop::Runtime::SyntaxNode
    def to_lisp
      [:quote, self.elements[0].to_lisp ]
    end
  end      

  class Nil < Treetop::Runtime::SyntaxNode
    def to_lisp
      [:quote, []]
    end
  end

end



  # class Lambda < Treetop::Runtime::SyntaxNode
  #   def to_lisp
  #     [:lambda] + self.elements.map { |e| e.to_lisp }
  #   end
  # end

  # class Define < Treetop::Runtime::SyntaxNode
  #   def to_lisp
  #     [:define] + self.elements.map { |e| e.to_lisp }
  #   end
  # end

  # class Params < Treetop::Runtime::SyntaxNode
  #   def to_lisp
  #     self.elements[0].to_lisp
  #   end
  # end

  # class Plist < Treetop::Runtime::SyntaxNode
  #   def to_lisp
  #     p self.elements
  #     self.elements.map { |e| e.to_lisp}
  #   end
  # end
