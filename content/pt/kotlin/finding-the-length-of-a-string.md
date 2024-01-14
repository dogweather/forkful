---
title:                "Kotlin: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum na programação e é importante para manipular adequadamente os dados em um programa. Aprender como fazer isso em Kotlin pode ser benéfico para melhorar suas habilidades de programação.

## Como fazer

Para encontrar o comprimento de uma string em Kotlin, podemos usar a função "length". Veja um exemplo abaixo:

```Kotlin
val str = "Olá mundo"
println(str.length)

// Output: 10
```

Neste exemplo, declaramos a string "Olá mundo" e usando a função "length" imprimimos seu comprimento, que é igual a 10.

Temos outros métodos que podem ser usados para encontrar o comprimento de uma string, como "count", "size" e "indices". Cada método tem suas próprias particularidades, portanto é importante entender a diferença entre eles.

## Aprofundando-se

A função "length" é usada para encontrar o tamanho de uma string, incluindo espaços em branco e caracteres especiais, mas excluindo linhas em branco. Já o método "count" retorna o número de caracteres em uma string, incluindo espaços em branco, caracteres especiais e linhas em branco.

O método "size" é semelhante a "length", mas só pode ser usado em strings mutáveis, que podem ser alteradas. Por fim, o método "indices" retorna um intervalo de índices da string.

Aproveite para experimentar esses métodos e veja como os resultados podem variar dependendo do tipo de string e método utilizado.

## Veja também

- [Documentação oficial do Kotlin sobre strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial sobre como trabalhar com strings em Kotlin](https://www.baeldung.com/kotlin/strings)
- [Exemplos práticos de strings em Kotlin](https://www.programiz.com/kotlin-programming/strings)