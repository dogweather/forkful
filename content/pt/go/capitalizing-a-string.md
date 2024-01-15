---
title:                "Transformando uma string em maiúscula"
html_title:           "Go: Transformando uma string em maiúscula"
simple_title:         "Transformando uma string em maiúscula"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Há muitas razões pelas quais você pode precisar capitalizar uma string em seus projetos de programação. Pode ser necessário para fins de formatação, classificação ou até mesmo para atender a requisitos específicos de um cliente. Independentemente do motivo, capitalizar uma string é uma tarefa comum na programação e conhecer as melhores práticas para fazê-lo pode economizar seu tempo e esforço.

## Como fazer em Go

Em Go, há várias maneiras de capitalizar uma string. Vamos dar uma olhada em algumas delas abaixo:

#### 1. Usando as funções embutidas do pacote "strings"

O pacote "strings" em Go possui funções embutidas que facilitam a capitalização de strings. Por exemplo, a função "Title" retornará uma cópia da string com a primeira letra de cada palavra em maiúscula. Aqui está um exemplo de como usá-la:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hello world"
	fmt.Println(strings.Title(str))
}

// Saída: Hello World
```

#### 2. Usando a biblioteca "strings" do pacote "unicode"

Se você precisar de mais controle sobre a capitalização de certos caracteres em uma string, a biblioteca "strings" do pacote "unicode" pode ser útil. Este exemplo mostra como usá-la para capitalizar a primeira letra de uma string:

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	str := "hello world"
	fmt.Println(strings.TitleFunc(str, unicode.ToUpper))
}

// Saída: Hello world
```

## Mergulho mais profundo

Uma observação importante ao capitalizar strings é que nem sempre é uma tarefa tão simples quanto parece. Dependendo do idioma e do contexto, pode haver várias maneiras de capitalizar uma palavra ou frase. Por exemplo, em alemão, a letra "ß" pode ser escrita como "ss" ao ser capitalizada. Portanto, é importante ter em mente essas nuances ao escrever código que lida com strings em diferentes idiomas ou situações.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre a capitalização de strings em Go:

- [Pacote strings](https://golang.org/pkg/strings/)
- [Pacote unicode](https://golang.org/pkg/unicode/)
- [Exemplos de capitalize em Go](https://programming.guide/go/capitalize-string.html)
- [Documentação oficial do Go](https://golang.org/doc/)