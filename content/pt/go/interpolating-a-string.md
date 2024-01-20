---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

---

## O Que é & Por Quê?

Interpolação de string é a inclusão de variáveis ou expressões dentro de uma string. Programadores o fazem para criar strings dinâmicas ou formatar a saída de strings convenientemente.

## Como Fazer:

Aqui está um exemplo básico de interpolação de string em Go:
```Go
package main

import (
	"fmt"
)

func main() {
  nome := "João"
  msg := fmt.Sprintf("Olá, %s!", nome)
  fmt.Println(msg)
}
```
E a saída será:
```Go
Olá, João!
```

## Aprofundamento

A interpolação de strings tem sido uma parte essencial da programação desde seus primeiros dias. Em Go, a função `fmt.Sprintf` é usada para a interpolação de strings. Ela retorna a string formatada de acordo com um formato especificado. Ela é semelhante a `printf`, mas retorna a string em vez de a imprimir.

Alternativas para interpolação de string em Go incluem `fmt.Printf` e `fmt.Fprintf`. `fmt.Printf` imprime a string formatada para a saída padrão. `fmt.Fprintf` grava para uma saída especificada, como um arquivo ou stream de rede.

A implementação da interpolação de strings em Go é bastante performática. Go converte a string e as variáveis em bytes para fins de processamento. Devido ao otimizado sistema de gerenciamento de memória de Go, isto é muito rápido e eficiente.

## Veja Também

A documentação oficial do pacote fmt: https://pkg.go.dev/fmt

Um guia excelente sobre a interpolação de string em Go: https://yourbasic.org/golang/string-format/

Uma discussão útil sobre interpolação de string vs concatenação em Go: https://stackoverflow.com/questions/1760757/how-to-efficiently-concatenate-strings-in-go