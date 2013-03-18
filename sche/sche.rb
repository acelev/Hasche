#!/usr/bin/env ruby 
#Scheme Intepreter
#Author Oren Leiman (oleiman@ucsc)
require 'treetop'
require_relative 'parser.rb'

def do_arith(op)
  ->(*args) { args.reduce(op) }
end

def do_bool(op)
  ->(*args) { 
    result = []
    args[0..-2].each_with_index do |a,i| 
      result << a.send(op, args[i+1])
    end
    result.all? { |b| b }
  }
end

# the global environment
$global_env = {\
  :+    => do_arith(:+),
  :*    => do_arith(:*),
  :-    => do_arith(:-),
  :'/'  => do_arith(:/),
  :'<'  => do_bool(:<),
  :'>'  => do_bool(:>),
  :car  => ->(xs)    { car(xs)      },
  :cdr  => ->(xs)    { cdr(xs)      },
  :cons => ->(x, xs) { cons(x, xs)  },
  :eq?  => ->(x, y)  { x == y       },
  :list => ->(*args) { args         }
}

# useful functions
def atom?(x)
  x.is_a?(Numeric)   || 
  x.is_a?(String)    ||
  x.is_a?(TrueClass) ||
  x.is_a?(FalseClass)
end

def variable?(x)
  x.is_a?(Symbol)
end

def car(xs)
  xs[0]
end

def cdr(xs)
  xs[1..-1]
end

def cons(x, xs)
  [x] + xs
end

def list?(xs)
  xs.is_a?(Array)
end

# creates a local environment without altering the enclosing.
# local bindings are created first, then enclosing bindings are
# merged in sans clobber.
def local_env (params, args, env)
  Hash[params.zip(args)].merge(env) { |k,v1,v2| v1 }
end

# TODO: abstract most of this out into a module(?) this file is
#       getting too large anyway.
def evalSch(expr, env)
  return expr      if atom?(expr)
  if variable?(expr) 
    return env[expr] unless env[expr].nil?
    raise NameError, "'#{expr}' is not defined"
  end

  case car(expr)
  when :quote
    return car(cdr(expr))
  when :lambda
    _, params, body = expr
    ->(*args) { 
      evalSch( body, local_env(params, args, env)) }
  when :define
    _, params, body = *expr
    if variable?(params)
      env[params] = evalSch(body, env)
    elsif params.all? { |p| variable?(p) }
      env[car(params)] = ->(*args) {
        evalSch(body, local_env(cdr(params), args, env)) }
      car(params).to_s
    end
  when :let
    _, asgns, stmt = *expr
    pairs = Hash[asgns]
    vals = pairs.values.map { |e| evalSch(e, env) }
    evalSch(stmt, local_env(pairs.keys, vals, env))
  when :if
      _, condition, conseq, altern = *expr
      if evalSch(condition, env)
        evalSch(conseq, env)
      else
        evalSch(altern, env)
      end
  else # procedure call
    proc = list?(car(expr)) ? evalSch(car(expr), env) : env[car(expr)]
    args = cdr(expr).map { |a| evalSch(a, env) }
    return proc.call(*args) unless proc.nil?
    raise NameError, "function '#{car(expr)}' not defined"
  end
end

## super-basic repl

def repl(prompt = '/~(8)~\ ')
  while true
    print prompt
    line = STDIN.gets.chomp
    break if line == '(quit)'
    begin
      parse = Parser.parse(line)
      value = evalSch(parse, $global_env)
      if value.is_a?(Proc) then puts "lambda" else emit(value) end
    rescue Exception => e
      puts e.message
      puts "something went wrong, somewhere..."
    end
  end
end

def emit(val)
  # add some code to process expressions for output...
  if list?(val)
    print "("
    val.each { |i| if i != val[-1] then print "#{i} " else print "#{i}" end}
    print ")\n"
  else
    print "=> #{val}\n"
  end
end

##run the thing
if ARGV[0] == '-i'
  repl()
elsif ARGV.size == 1 and ARGV[0] != "spec"
  print "=> "
  parse = Parser.parse(ARGV[0])
  p evalSch(parse, $global_env) 
end

