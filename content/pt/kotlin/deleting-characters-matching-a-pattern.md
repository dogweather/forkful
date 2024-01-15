---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Kotlin: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Você pode querer deletar caracteres que correspondam a um padrão em uma string para filtrar e manipular dados em uma aplicação Kotlin.

## Como Fazer

```Kotlin
val string = "HelloKotlin!"
val pattern = "[A-Z]".toRegex() // criando uma expressão regular para encontrar letras maiúsculas
val result = string.replace(pattern, "") // substituindo todas as letras maiúsculas por uma string vazia
println(result) // output: "otlin!"
```

## Deep Dive

Para deletar caracteres que correspondam a um padrão em uma string, você precisará seguir três etapas principais:

1. Criar uma expressão regular com o padrão desejado utilizando a classe `Regex` ou o método `toRegex()`.
2. Utilizar o método `replace()` da classe `String`, passando a expressão regular e a string de substituição como parâmetros.
3. Armazenar o resultado da substituição em uma nova variável ou imprimi-lo diretamente utilizando `println()`.

Você pode aprender mais sobre expressões regulares e suas sintaxes na documentação oficial do Kotlin.

## Veja Também

- [Documentação Oficial do Kotlin: Expressões Regulares](https://kotlinlang.org/docs/regex.html)
- [Tutorial de Expressões Regulares em Kotlin](https://www.javacodegeeks.com/2017/05/regular-expressions-kotlin.html)
- [Tutorial de Expressões Regulares em Kotlin: Programação Reativa](https://www.thomasnield.com/blog/2017/11/11/filtering-sequences-with-regular-expressions-for-programmers-introducing-reactive-programming-with-kotlin)