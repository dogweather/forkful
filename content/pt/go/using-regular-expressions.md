---
title:                "Go: Aplicando expressões regulares"
simple_title:         "Aplicando expressões regulares"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares em Go?

As expressões regulares são uma ferramenta poderosa para fazer correspondência e manipulação de padrões de texto em uma ampla variedade de linguagens de programação. Quando se trata de Go, as expressões regulares podem ser usadas para validar dados de entrada, fazer buscas em arquivos e até mesmo substituir partes de uma string. Portanto, se você está trabalhando com manipulação de texto em seu projeto Go, as expressões regulares podem ser muito úteis.

## Como usar expressões regulares em Go

Para utilizar expressões regulares em um código Go, o primeiro passo é importar o pacote `regexp` em seu arquivo:

```Go
import "regexp"
```

Em seguida, você pode criar uma expressão regular utilizando a função `Compile()` do pacote `regexp`:

```Go
re := regexp.MustCompile("go(lang)?")
```

Neste exemplo, a expressão regular irá corresponder a qualquer string que contenha "go" ou "golang". O `?` indica que o trecho "lang" é opcional.

Para fazer correspondências em uma string, utilize a função `MatchString()` da expressão regular, passando a string que deseja verificar:

```Go
fmt.Println(re.MatchString("I love Go!"))
// Saída: true
```

Você também pode utilizar o método `FindString()` para encontrar a primeira ocorrência da expressão regular em uma string e retornar a correspondência:

```Go
fmt.Println(re.FindString("Golang is the best!"))
// Saída: golang
```

Para substituir uma parte da string que corresponde à expressão regular, pode-se utilizar o método `ReplaceAllString()`:

```Go
newString := re.ReplaceAllString("Let's learn Go!", "Learned")
fmt.Println(newString)
// Saída: Let's Learned Go!
```

Fique à vontade para explorar outras funções e métodos disponíveis no pacote `regexp` para lidar com suas necessidades específicas de expressões regulares em Go.

## Aprofundando nas Expressões Regulares em Go

As expressões regulares em Go seguem a sintaxe do pacote `regexp` do padrão POSIX. Isso significa que a maioria dos metacaracteres comuns em expressões regulares, como `*` e `+`, devem ser escapados com barra invertida no código Go.

Além disso, para lidar com caracteres especiais em strings, você pode utilizar o pacote `regexp/syntax` do Go para definir um contexto de codificação, que irá ajudar na correspondência correta em diferentes situações.

Para mais informações e exemplos, confira a documentação oficial do pacote `regexp` do Go e o tutorial em português sobre o tema no site Projeto Golang (link abaixo).

## Veja também

- [Documentação oficial do pacote `regexp` do Go](https://golang.org/pkg/regexp/)
- [Tutorial sobre Expressões Regulares em Go no Projeto Golang](https://tutorialedge.net/golang/go-regex-tutorial/)