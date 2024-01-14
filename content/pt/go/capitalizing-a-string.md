---
title:                "Go: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Go
Capitalizar uma string em Go é uma tarefa comum em muitos programas, especialmente em projetos de processamento de linguagem natural ou na formatação de dados de entrada. Através da capitalização, podemos padronizar a forma como strings são exibidas ou comparadas, tornando o código mais legível e consistente.

## Como Capitalizar uma string em Go

Para capitalizar uma string em Go, podemos utilizar a função `strings.Title()`, que está incluída no pacote padrão `strings`. Vamos ver um exemplo simples de como usar essa função:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "este é um exemplo de string em Go."
    fmt.Println(strings.Title(str))
}
```

O código acima irá imprimir a string "Este é um Exemplo de String em Go.". A função `strings.Title()` irá capitalizar a primeira letra de cada palavra na string, conforme o padrão de capitalização utilizado em inglês.

Podemos também utilizar a função `strings.ToTitle()` para capitalizar todas as letras em uma string:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "este é um exemplo de string em Go."
    fmt.Println(strings.ToTitle(str))
}
```

O resultado será "ESTE É UM EXEMPLO DE STRING EM GO.", com todas as letras maiúsculas.

## Aprofundando em Capitalização de Strings em Go

Para aqueles interessados em entender melhor como a função `strings.Title()` funciona, é importante notar que ela utiliza o pacote `unicode` para determinar quais caracteres devem ser capitalizados. Por exemplo, no idioma português, as letras "Ç" e "ã" também devem ser capitalizadas no início da palavra. No entanto, no momento em que esse artigo foi escrito, a função `strings.Title()` não suporta o idioma português.

Para contornar esse problema, podemos utilizar o pacote `github.com/golang/text` e especificar o idioma que desejamos usar na função `Title()`. Aqui está um exemplo de como capitalizar uma string em português brasileiro, utilizando a função `Title()` do pacote `text`:

```Go
package main

import (
    "fmt"
    "github.com/golang/text/language"
    "github.com/golang/text/language/unicode"
    "github.com/golang/text/transform"
    "github.com/golang/text/unicode/norm"
)

func main() {
    str := "este é um exemplo de string em Go."
    t := transform.Chain(norm.NFD, unicode.CaseMapper(language.BrazilianPortuguese), norm.NFC)
    result, _, _ := transform.String(t, str)
    fmt.Println(result)
}
```

O resultado será "Este é um Exemplo de String em Go.", com as letras "Ç" e "ã" devidamente capitalizadas.

## Veja Também
- [Documentação da função strings.Title() em Go](https://golang.org/pkg/strings/#Title)
- [Documentação da função strings.ToTitle() em Go](https://golang.org/pkg/strings/#ToTitle)
- [Pacote strings no GoDoc](https://golang.org/pkg/strings/)
- [Pacote unicode no GoDoc](https://golang.org/pkg/unicode/)
- [Pacote text no GoDoc](https://godoc.org/golang.org/x/text)