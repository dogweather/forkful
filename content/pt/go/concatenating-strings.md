---
title:                "Go: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em Go?

Concatenar strings é uma tarefa comum na programação e pode ser especialmente útil em situações em que precisamos combinar diferentes partes de texto em uma única string. Em Go, essa operação é realizada através do operador de adição `+` ou do pacote `strings` da biblioteca padrão.

## Como fazer em Go

Para concatenar strings em Go, podemos usar o operador `+` como mostrado no exemplo abaixo:

```Go
texto1 := "Olá"
texto2 := "mundo!"
resultado := texto1 + " " + texto2
fmt.Println(resultado)

//Output: Olá mundo!
```

Também podemos usar o pacote `strings` para realizar concatenação usando a função `Join`:

```Go
textos := []string{"Olá", "mundo!"}
resultado := strings.Join(textos, " ")
fmt.Println(resultado)

//Output: Olá mundo!
```

## Mergulho profundo

Ao concatenar strings usando o operador `+`, devemos ter cuidado com o desempenho do nosso código, pois essa operação cria uma nova string que contém as duas strings originais. Em situações em que precisamos concatenar muitas strings, isso pode levar a um aumento significativo no uso de memória e impactar o desempenho geral do programa.

Uma forma mais eficiente de concatenar strings é usando o pacote `strings.Builder` da biblioteca padrão. Este tipo armazena as strings em um buffer e as concatena de forma mais otimizada, sem a necessidade de criar uma nova string a cada vez.

Outro ponto importante a ser observado é que a concatenação de strings em Go é imutável, ou seja, as strings originais não são alteradas, apenas uma nova string é criada com o resultado da concatenação. Por isso, é importante sempre armazenar o resultado em uma nova variável ou usar o valor de retorno da função `Join`.

## Veja também

- [Documentação oficial do pacote strings em Go](https://golang.org/pkg/strings/)
- [Tutorial de strings em Go](https://www.calhoun.io/concatenating-and-building-strings-in-go/)
- [Código fonte dos exemplos apresentados neste artigo](https://github.com/example/example)