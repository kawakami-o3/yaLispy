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

def add_globals env
  Math.methods.each do |i| 
    env.update({i.to_s=>Math.method(i)})
  end
  [{"+"=>lambda {|x,y| x + y}} ,
    {"-"=>lambda {|x,y| x - y}} ,
    {"*"=>lambda {|x,y| x * y}} ,
    {"/"=>lambda {|x,y| x / y}} ,
    {"not"=>lambda {|x| not x}} ,
    {">"=>lambda {|x,y| x > y}} ,
    {"<"=>lambda {|x,y| x < y}} ,
    {"<"=>lambda {|x,y| x < y}} ,
    {">="=>lambda {|x,y| x >= y}} ,
    {"<="=>lambda {|x,y| x <= y}} ,
    {"="=>lambda {|x,y| x == y}} ,
    {"equal?"=>lambda {|x,y| x == y}} ,
    {"eq?"=>lambda {|x,y| x.__id__ == y.__id__}} ,
    {"length"=>lambda {|x| x.length}} ,
    {"cons"=>lambda {|x,y| [x] + y}} ,
    {"car"=>lambda {|x| x[0]}} ,
    {"cdr"=>lambda {|x| x[1..-1]}} ,
    {"append"=>lambda {|x,y| x + y}} ,
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

def eval x , env=Global_env
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
    return eval(eval(test, env) ? conseq : alt, env)
  elsif x[0] == "set!"
    var = x[1]
    exp = x[2]
    env.find(var)[var] = eval(exp,env)
  elsif x[0] == "define"
    var = x[1]
    exp = x[2]
    env[var] = eval(exp,env)
  elsif x[0] == "lambda"
    vars = x[1]
    exp = x[2]
    return lambda {|*args| eval(exp, Env.new(vars, args, env))}
  elsif x[0] == "begin"
    ret = nil
    x[1..-1].each do |exp|
      ret = eval(exp, env)
    end
    return ret
  else
    exps = x.map{|i| eval(i,env)}
    prc = exps.shift
    return prc.call(*exps)
  end
end


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
  exp.class == Array ? "(#{exp.map{|i| to_string(i)}.join(" ")})" : exp.to_s
end

def repl prompt="#{__FILE__}> "
  while buf = Readline.readline(prompt, true)
    val = eval(parse(buf))
    puts "-> #{to_string(val)}" unless val == nil
  end
end


if $0 == __FILE__
  repl()
end

