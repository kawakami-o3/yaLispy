package main

import (
	"bufio"
	"fmt"
	"os"
)

const (
	Prompt     = "lisp.go> "
	PromptNext = "-> "
)

func parse(s string) string {
	return "-"
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	fmt.Print(Prompt)
	for scanner.Scan() {
		s := scanner.Text()
		if len(s) > 0 {
			fmt.Println(parse(s))
			fmt.Print(Prompt)
		}
	}
}
