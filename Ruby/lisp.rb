#!/usr/bin/env ruby

require 'readline'

class Env < Hash
  def initialize parms=[], args=[], outer=nil
    parms.zip(args).each do |k,v|
      self[k] = v
    end
    @outer = outer
  end

  attr_reader :outer

  def find var
    return has_key?(var) ? self : @outer.find(var)
  end
end

def infix label, sym=nil
  sym = :"#{label}" if sym == nil
  {label => lambda {|x,y| x.method(sym).call(y) }}
end

def add_globals env
  Math.methods.each do |i| 
    env.update({i.to_s=>Math.method(i)})
  end
  [infix("+") , infix("-") , infix("*") , infix("/") ,
    infix(">") , infix("<") , infix(">=") , infix("<=") ,
    infix("=", :==) , {"not"=>lambda {|x| not x}} ,
    infix("append", :+), infix("equal?", :==) ,
    {"eq?"=>lambda {|x,y| x.__id__ == y.__id__}} ,
    {"length"=>lambda {|x| x.length}} ,
    {"cons"=>lambda {|x,y| [x] + y}} ,
    {"car"=>lambda {|x| x[0]}} ,
    {"cdr"=>lambda {|x| x[1..-1]}} ,
    {"list"=>lambda {|*x| x}} ,
    {"list?"=>lambda {|x| x.class == Array}} ,
    {"null?"=>lambda {|x| x == []}} ,
    {"symbol?"=>lambda {|x| x.class == String}}
  ].each do |h|
    env.update(h)
  end
  return env
end

Global_env = add_globals(Env.new())

# --------------------------------------------------------------

def eval_sexp x , env=Global_env
  if x.class == String
    return env.find(x)[x]
  elsif x.class != Array
    return x
  elsif x[0] == "quote"
    exp = x[1]
    return exp
  elsif x[0] == "if"
    test = x[1]
    conseq = x[2]
    alt = x[3]
    return eval_sexp(eval_sexp(test, env) ? conseq : alt, env)
  elsif x[0] == "set!"
    var = x[1]
    exp = x[2]
    env.find(var)[var] = eval_sexp(exp,env)
  elsif x[0] == "define"
    var = x[1]
    exp = x[2]
    env[var] = eval_sexp(exp,env)
  elsif x[0] == "lambda"
    vars = x[1]
    exp = x[2]
    return lambda {|*args| eval_sexp(exp, Env.new(vars, args, env))}
  elsif x[0] == "begin"
    ret = nil
    x[1..-1].each do |e|
      ret = eval_sexp(e, env)
    end
    return ret
  else
    exps = x.map{|i| eval_sexp(i,env)}
    prc = exps.shift
    return prc.call(*exps)
  end
end

# --------------------------------------------------------------

def read_from tokens
  raise SyntaxError.new("unexpected EOF while reading") if tokens.length == 0
  token = tokens.shift
  if "(" == token
    ret = []
    while tokens.first != ")"
      ret << read_from(tokens)
    end
    tokens.shift
    return ret
  elsif ")" == token
    raise SyntaxError.new("unexpected )")
  else
    return atom(token)
  end
end

def tokenize s
  s.gsub(/\(/," ( ").gsub(/\)/," ) ").split
end

def read s
  read_from(tokenize(s))
end

def parse s
  read s
end

def atom token
  begin
    return Integer(token)
  rescue ArgumentError
    begin
      return Float(token)
    rescue
      return String(token)
    end
  end
end

def to_string exp
	if exp.class == Array
	 	"(#{exp.map{|i| to_string(i)}.join(" ")})"
	else
		exp.to_s
	end
end


def diff_bracket buf
	buf.count("(") - buf.count(")")
end

def read_sentence prompt
	buf = Readline.readline(prompt, true)
	if buf == nil || buf.length == 0
		return buf
	else
		while diff_bracket(buf) > 0
			buf += Readline.readline("", true)
		end

		if diff_bracket(buf) == 0
			return buf
		else # "(" < ")"
			puts "invalid"
			return ""
		end
	end
end

def repl prompt="#{__FILE__}> "
  #while buf = Readline.readline(prompt, true)
  while buf = read_sentence(prompt)
		if buf == ""
			next
		end
    val = eval_sexp(parse(buf))
    puts "-> #{to_string(val)}" unless val == nil
  end
end


if $0 == __FILE__
  repl()
end

