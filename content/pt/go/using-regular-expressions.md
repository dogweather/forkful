---
title:                "Utilizando expressões regulares"
html_title:           "Go: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Go?

As expressões regulares são uma ferramenta poderosa para manipulação de texto em qualquer linguagem de programação. Em Go, elas oferecem uma maneira concisa e eficiente de realizar a busca e manipulação de padrões em strings. Com as expressões regulares, é possível economizar tempo e esforço na manipulação e validação de dados.

## Como usar expressões regulares em Go?

Em Go, as expressões regulares são suportadas pela biblioteca padrão "regexp". Para começar a utilizá-las, basta importar essa biblioteca em seu código:

```Go 
import "regexp"
```

Em seguida, podemos compilar a expressão regular desejada utilizando a função `Compile()`:

```Go
re := regexp.Compile("lo+")
```

Esta expressão regular irá procurar por strings que contenham um ou mais "o" consecutivos, como "loo" ou "loooo". Agora, podemos utilizar a função `MatchString()` para verificar se um texto corresponde à nossa expressão regular:

```Go
re.MatchString("looong") // retorna true
re.MatchString("test") // retorna false
```

Além disso, também é possível utilizar as expressões regulares para fazer substituições de padrões em uma string. A função `ReplaceAllString()` recebe três argumentos: a string original, o padrão a ser substituído e o novo texto a ser inserido:

```Go
re.ReplaceAllString("carro", "banco") // retorna "carroo"
```

## Aprofundando em expressões regulares em Go

Em Go, é possível utilizar algumas sequências de escape especiais para especificar padrões mais complexos em uma expressão regular. Por exemplo, a sequência `\d` irá procurar por qualquer dígito de 0 a 9, enquanto `\w` irá procurar por qualquer caracter alfanumérico. Além disso, também é possível utilizar quantificadores, como `*` (zero ou mais ocorrências) e `+` (uma ou mais ocorrências).

Outra forma de utilizar as expressões regulares em Go é através da função `FindAllString()`, que retorna todas as ocorrências de um padrão em uma string como um array.

É importante ressaltar que o uso correto de expressões regulares pode ser um pouco complexo e exigir prática para se tornar eficiente. Por isso, é recomendado ler a documentação da biblioteca "regexp" e praticar com diferentes padrões para se familiarizar com sua utilização em Go.

## Veja também

- [Documentação da biblioteca "regexp" em Go](https://golang.org/pkg/regexp/)
- [Tutorial sobre expressões regulares em Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-pt)