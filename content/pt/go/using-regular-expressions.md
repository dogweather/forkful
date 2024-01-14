---
title:                "Go: Usando expressões regulares."
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Go?

As expressões regulares são uma poderosa ferramenta para manipulação de textos. Elas permitem que um programador busque e manipule padrões específicos em uma string. Em Go, as expressões regulares são suportadas pela biblioteca "regexp", e podem ser úteis em diversas situações, como validação de entradas do usuário, extração de dados de textos e até mesmo em tarefas de web scraping.

## Como usar expressões regulares em Go?

Usar expressões regulares em Go é simples e direto. Primeiro, devemos importar a biblioteca "regexp" em nosso código. Em seguida, podemos criar um objeto de expressão regular utilizando a função `Compile()` e passando como parâmetro a expressão que desejamos buscar. Por exemplo:

```
package main

import (
    "fmt"
    "regexp"
)

func main() {
    r := regexp.MustCompile(`Gol`)
    result := r.MatchString("Golang é uma linguagem de programação poderosa.")

    fmt.Println(result) // Output: true
}
```

No exemplo acima, criamos uma expressão regular que busca por qualquer palavra que contenha as letras "Gol", e utilizamos a função `MatchString()` para verificar se a string informada possui algum match. Podemos também utilizar a função `FindString()` para obter a primeira ocorrência do padrão encontrado na string.

## Aprofundando-se em expressões regulares em Go

Além das funções já mencionadas, a biblioteca "regexp" em Go oferece diversas outras opções para trabalharmos com expressões regulares, como por exemplo:

- `FindAllString()`: retorna uma lista de todas as ocorrências do padrão encontrado na string;
- `Submatch()`: retorna as subpartes da string que correspondem aos grupos de captura definidos na expressão regular;
- `ReplaceAllString()`: substitui todas as ocorrências do padrão encontrado por outro valor.

É importante ressaltar também que, assim como em outras linguagens de programação, em Go também podemos utilizar metacaracteres em nossas expressões regulares, que permitem buscar padrões mais complexos e evitam que tenhamos que escrever uma expressão longa e específica.

## Veja também

- [Documentação oficial sobre expressões regulares em Go](https://golang.org/pkg/regexp/)
- [Artigo sobre o uso de metacaracteres em expressões regulares em Go](https://www.sohamkamani.com/blog/golang/2019-01-06-golang-regex-tutorial/)