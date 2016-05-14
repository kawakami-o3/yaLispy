package main

// https://gist.github.com/ofan/721464

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var GLOBAL_ENV = NewGlobalEnvironment()
var NIL = NewAtom("nil")
var TRUE = NewAtom("#t")
var FALSE = NewAtom("#f")

type Environment struct {
	dictionary map[string]*Cell
	outer      *Environment
}

func NewEnvironment(params []*Cell, args []*Cell, outer *Environment) *Environment {
	m := map[string]*Cell{}
	for i, c := range params {
		m[c.value] = args[i]
	}
	return &Environment{m, outer}
}

func NewGlobalEnvironment() *Environment {
	m := map[string]*Cell{}

	m[NIL.value] = NIL
	m[TRUE.value] = TRUE
	m[FALSE.value] = FALSE

	m["append"] = NewProc(func(a *Cell) *Cell {
		ret := a.list[0]
		for i := 1; i < len(a.list); i++ {
			for _, c := range a.list[i].list {
				ret.Append(c)
			}
		}
		return ret
	})
	m["car"] = NewProc(func(a *Cell) *Cell {
		return a.list[0].list[0]
	})
	m["cdr"] = NewProc(func(a *Cell) *Cell {
		ret := NewList()
		list := a.list[0].list
		for i := 1; i < len(list); i++ {
			ret.Append(list[i])
		}
		return ret
	})
	m["cons"] = NewProc(func(a *Cell) *Cell {
		ret := NewList()
		ret.Append(a.list[0])
		for _, c := range a.list[1].list {
			ret.Append(c)
		}
		return ret
		return nil
	})
	m["length"] = NewProc(func(a *Cell) *Cell {
		return NewAtom(strconv.Itoa(len(a.list[0].list)))
	})
	m["list"] = NewProc(func(a *Cell) *Cell {
		ret := NewList()
		for _, c := range a.list {
			ret.Append(c)
		}
		return ret
	})
	m["list?"] = NewProc(func(a *Cell) *Cell {
		return NewBool(a.list[0].is(TYPE_LIST))
	})
	m["symbol?"] = NewProc(func(a *Cell) *Cell {
		return NewBool(a.list[0].is(TYPE_SYMBOL))
	})
	m["null?"] = NewProc(func(a *Cell) *Cell {
		return NewBool(a.list[0] == NIL)
	})
	m["not"] = NewProc(func(a *Cell) *Cell {
		return NewBool(a.list[0].Equal(a.list[1]))
	})

	m["+"] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for i := 1; i < len(a.list); i++ {
			n += a.list[i].toInt()
		}
		return NewNumber(n)
	})

	m["-"] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for i := 1; i < len(a.list); i++ {
			n -= a.list[i].toInt()
		}
		return NewNumber(n)
	})
	m["*"] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for i := 1; i < len(a.list); i++ {
			n *= a.list[i].toInt()
		}
		return NewNumber(n)
	})
	m["/"] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for i := 1; i < len(a.list); i++ {
			n /= a.list[i].toInt()
		}
		return NewNumber(n)
	})
	m[">"] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for _, v := range a.list[1 : len(a.list)-1] {
			if false == (n > v.toInt()) {
				return NewBool(false)
			}
		}
		return NewBool(true)
	})
	m["<"] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for _, v := range a.list[1 : len(a.list)-1] {
			if false == (n < v.toInt()) {
				return NewBool(false)
			}
		}
		return NewBool(true)
	})
	m["<="] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for _, v := range a.list[1 : len(a.list)-1] {
			if false == (n <= v.toInt()) {
				return NewBool(false)
			}
		}
		return NewBool(true)
	})
	m[">="] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for _, v := range a.list[1 : len(a.list)-1] {
			if false == (n >= v.toInt()) {
				return NewBool(false)
			}
		}
		return NewBool(true)
	})
	m["="] = NewProc(func(a *Cell) *Cell {
		n := a.list[0].toInt()
		for _, v := range a.list[1 : len(a.list)-1] {
			if false == (n == v.toInt()) {
				return NewBool(false)
			}
		}
		return NewBool(true)
	})

	return &Environment{m, nil}
}

func NewBool(cond bool) *Cell {
	if cond {
		return TRUE
	} else {
		return FALSE
	}
}

func (e *Environment) Find(name string) *Environment {
	atom := e.dictionary[name]
	if atom != nil {
		return e
	} else if e.outer != nil {
		return e.outer.Find(name)
	} else {
		return nil
	}
}

func Eval(cell *Cell, env *Environment) *Cell {
	if cell.is(TYPE_SYMBOL) {
		return env.Find(cell.value).dictionary[cell.value]
	} else if cell.is(TYPE_NUMBER) {
		return cell
	} else if len(cell.list) == 0 {
		return NIL
	} else if cell.list[0].is(TYPE_SYMBOL) {
		head := cell.list[0].value
		if head == "quote" {
			return cell.list[1]
		} else if head == "if" {
			if Eval(cell.list[1], env).value == FALSE.value {
				if len(cell.list) < 4 {
					return NIL
				} else {
					return Eval(cell.list[3], env)
				}
			} else {
				return Eval(cell.list[2], env)
			}
		} else if head == "set!" {
			c := Eval(cell.list[2], env)

			key := cell.list[1].value
			e := env.Find(key)
			if e == nil {
				e = env
			}
			e.dictionary[key] = c
			return c
		} else if head == "define" {
			c := Eval(cell.list[2], env)
			key := cell.list[1].value
			env.dictionary[key] = c
			return c
		} else if head == "lambda" {
			cell.cellType = TYPE_LAMBDA
			cell.env = env
			return cell
		} else if head == "begin" {
			for i := 1; i < len(cell.list)-1; i++ {
				Eval(cell.list[i], env)
			}
			return Eval(cell.list[len(cell.list)-1], env)
		}
	}

	proc := Eval(cell.list[0], env)
	exps := NewList()
	for i := 1; i < len(cell.list); i++ {
		exps.Append(Eval(cell.list[i], env))
	}

	if proc.cellType == TYPE_LAMBDA {
		return Eval(proc.list[2], NewEnvironment(proc.list[1].list, exps.list, env))
	} else if proc.cellType == TYPE_PROC {
		return proc.proc(exps)
	} else {
		println("not a function")
		return NIL
	}
}

const (
	TYPE_SYMBOL = iota + 1
	TYPE_NUMBER
	TYPE_LIST
	TYPE_PROC
	TYPE_LAMBDA
)

type Cell struct {
	cellType int
	value    string
	list     []*Cell
	proc     func(*Cell) *Cell

	env *Environment
}

func NewAtom(v string) *Cell {
	c := &Cell{TYPE_SYMBOL, v, nil, nil, nil}
	_, err := strconv.Atoi(v)
	if err == nil {
		c.cellType = TYPE_NUMBER
	}
	return c
}

func NewNumber(num int) *Cell {
	return &Cell{TYPE_NUMBER, strconv.Itoa(num), nil, nil, nil}
}

func NewList() *Cell {
	return &Cell{TYPE_LIST, "", []*Cell{}, nil, nil}
}

func NewProc(proc func(*Cell) *Cell) *Cell {
	return &Cell{TYPE_PROC, "", nil, proc, nil}
}

func (c *Cell) is(typeId int) bool {
	return c.cellType == typeId
}

func (c *Cell) Append(a *Cell) {
	c.list = append(c.list, a)
}

func (c *Cell) toInt() int {
	i, err := strconv.Atoi(c.value)
	if err != nil {
		panic(err)
	}
	return i
}

func (c *Cell) Equal(a *Cell) bool {
	return c.ToString() == a.ToString()
}

func (c *Cell) ToString() string {
	switch c.cellType {
	case TYPE_LIST:
		s := "("
		for i := 0; i < len(c.list); i++ {
			s += " " + c.list[i].ToString()
		}
		s += " )"
		return s
	case TYPE_LAMBDA:
		return "<Lambda>"
	case TYPE_PROC:
		return "<Proc>"
	default:
		return c.value
	}
}

type TokenBuffer struct {
	tokens []string
	index  int
}

func NewTokenBuffer(tokens []string) *TokenBuffer {
	return &TokenBuffer{tokens, 0}
}

func (buf *TokenBuffer) Shift() string {
	ret := buf.tokens[buf.index]
	buf.index++
	return ret
}

func (buf *TokenBuffer) Get() string {
	return buf.tokens[buf.index]
}

func Tokenize(s string) *TokenBuffer {
	pairs := [][]string{
		{`\(`, " ( "},
		{`\)`, " ) "},
		{` +`, " "},
		{` +$`, ""},
		{`^ +`, ""},
	}

	ret := s
	for _, e := range pairs {
		ret = regexp.MustCompile(e[0]).ReplaceAllString(ret, e[1])
	}
	return NewTokenBuffer(strings.Split(ret, " "))
}

func ReadFrom(buf *TokenBuffer) *Cell {
	if len(buf.tokens) == 0 {
		panic("unexpected EOF while reading")
	}

	token := buf.Shift()

	if "(" == token {
		ret := NewList()
		for buf.Get() != ")" {
			ret.Append(ReadFrom(buf))
		}
		buf.Shift()
		return ret
	} else if ")" == token {
		panic("unexpected )")
	} else {
		return NewAtom(token)
	}
}

func Parse(s string) *Cell {
	return ReadFrom(Tokenize(s))
}

func main() {

	const (
		Prompt     = "lisp.go> "
		PromptNext = "-> "
	)

	scanner := bufio.NewScanner(os.Stdin)
	fmt.Print(Prompt)
	for scanner.Scan() {
		s := scanner.Text()
		if len(s) > 0 {
			fmt.Println(Eval(Parse(s), GLOBAL_ENV).ToString())
			fmt.Print(Prompt)
		}
	}

}
