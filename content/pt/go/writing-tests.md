---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Escrever testes é criar um conjunto de procedimentos para verificar se o código funciona como esperado. Programadores testam para prevenir bugs, garantir a qualidade e facilitar atualizações.

## Como fazer:

```Go
package main

import (
	"testing"
)

func Soma(x int, y int) int {
	return x + y
}

func TestSoma(t *testing.T) {
	resultado := Soma(5, 5)
	esperado := 10
	if resultado != esperado {
		t.Errorf("Soma(5, 5) = %d; esperado %d", resultado, esperado)
	}
}
```

Para rodar o teste, execute:

```Shell
$ go test
```

Saída esperada:

```Shell
PASS
ok  	nome_do_pacote	0.001s
```

## Mergulho Profundo

Testes em Go foram inspirados por outros frameworks de teste como xUnit. Alternativas incluem tabelas de teste para casos múltiplos e "benchmarks" para análise de desempenho. A biblioteca padrão `testing` provê a maior parte das ferramentas necessárias para escrever testes unitários eficientes.

## Veja Também

- Documentação oficial do Go: https://golang.org/pkg/testing/
- Artigo sobre test-driven development (TDD) em Go: https://medium.com/go-community/tdd-in-go-53839ff85c36
- Go testing package at Go by Example: https://gobyexample.com/testing