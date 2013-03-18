#!/usr/bin/env python

#Author
# Ace Levenberg (llevenbe@ucsc.edu)

import operator as op
import sys

def isAtom(x):
   if type(x) == str and x[0] == '\"':
      return True
   return type(x) == int or type(x) ==  float or type (x) == bool

def var(x):
   return type(x) == str

def car(xs):
   return xs[0]

def cdr(xs):
   return xs[1:]

class Env(dict):
   def __init__(self, params = [], args = [], parent=None):
      self.update(parent)
      self.update(zip(params,args))
   def lookup(self,var):
      try:
         return self[var] 
      except LookupError:
         raise LookupError("dude, you didn't define", var)

   def insert(self, var, value):
      self[var] = value

def operator(op):
   return lambda *args : reduce(op, args)             
 
global_env = Env( parent = {'+':operator(op.add), '-':operator(op.sub), 
                           '/':operator(op.div), 
                           '*':operator(op.mul), 'car':car, 'cdr':cdr,
                           '>':op.gt, '<':op.lt, '>=':op.ge, 
                           '<=':op.le, '!=':op.ne, 'eq?':op.eq,
                           'null?': lambda x : op.eq([], x),
                           'list?': lambda x : type(x) is list, 
                           'cons': lambda x, y : [x] + y
                           })



def eval(expr, env=global_env):
   if isAtom(expr):
      return expr
   elif var(expr):
      return env.lookup(expr)  
   #get the front of the expr
   head = car(expr)
   if head == 'quote':
      return car(cdr(expr))
   elif head == 'lambda':
      (_, params, body) = expr
      # return a lambda function with the proper environment 
      #with proper params added
      return lambda *args: eval(body,Env(params, args, env)) 
   elif head == 'define':
      (_, params, body) = expr
      #insert a function with the proper 
      if (len(params) == 1):
         env.insert(car(params), eval(body, env))
      else:
         env.insert(car(params), 
         lambda *args: eval(body, Env(cdr(params), args, env)))
      return env.lookup(car(params))
   elif head == 'let':
      (_, vals, stm) = expr
      #maps eval()  all the values in expression 
      #creates a tuple with key and value
      #takes list of tuples and casts it to a dictionary
      pairs = dict(map(lambda val : (car(val), eval(val[1], env)), vals)) 
      return eval(stm, Env(pairs.keys(), pairs.values(), env))
   elif head == 'list': 
     evaled = map(lambda exp : eval(exp, env), (cdr(expr)))
     return evaled 
   elif head == 'if':
      stms = cdr(expr)
      if eval(car(stms), env):
         return eval (car(cdr(stms)), env)
      elif len(cdr(cdr(stms))) != 0:
         return eval (car(cdr(cdr(stms))), env)
      else:
         return 
   #proc 
   else:
      args = []
      try:
         for arg in cdr(expr):
            args.append(eval(arg, env)) 
         return env.lookup(car(expr))(*args) 
      except TypeError:
         #join all the types of the params into a string 
         raise TypeError("broham...", car(expr), 
         "doesn't work on", " and ".join(map(lambda x : str(type(x)),args)))

#norvig Atom(x) 
def toAtom(x):
   try: return int(x)
   except ValueError:
      try: return float(x)
      except ValueError:
         return x

def parse(tokens):
   token = tokens.pop(0)
   # if we are starting a new "cons" element
   if token == '(': 
      #construct the cons element into a list
      program = []
      try:
         while tokens[0] != ')':
            #until we hit the end of the list append 
            program.append(parse(tokens)) 
         tokens.pop(0)
         return program 
      except IndexError, TypeError:
         raise IndexError("whoa bro check yer program")
   elif token == ')':
      print "to many )'s bro ham"
   elif token == '\'':
      return ['quote', parse(tokens)] 
   # nil is sugar for empty list, so at parse time it is changed
   elif token == 'nil':
      return ['quote', []]
   elif token == '#t':
      return True
   elif token == '#f':
      return False
   else :
      return toAtom(token) 

def tokenize(line):
   return line.replace('(', ' ( ').replace(')' , ' ) ').split()

def printArgs(*args):
   #raiseing errors has a tuple within a tuple... join all of the strings with " " 
   if len(args) >= 1:
      print " ".join(list(list(args)[0]))

def repl(prompt = "Whats Good bro? >>: "):
   while True:
      line = raw_input(prompt)
      if line == "(done bro)":
         print "Alright dude chill"
         break
      try:
         program =  parse(tokenize(line))
         #special case for lambdas
         if len(program) >= 2 and program[0][0] == "lambda":
            print "=> " , eval(program[0])(*map(eval, program[1:]))
         else:
            print "=> ",  eval(program)
         
      except IndexError, err:
         print line, err.args[0]
      except TypeError, err:
         #print err
         printArgs(err.args)
      except LookupError, err:
         #print err
         printArgs(err.args)
      except ValueError:
         print line, "didn't call that right broseph" 

if __name__ == "__main__":
   repl() 
