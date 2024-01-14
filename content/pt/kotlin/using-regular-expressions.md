---
title:                "Kotlin: Utilizando expressões regulares"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Kotlin?

As expressões regulares são uma ferramenta poderosa para manipular padrões de texto em uma linguagem de programação. Em Kotlin, elas podem ser usadas para validar, extrair e substituir partes de uma string. Se você trabalha com manipulação de texto em suas aplicações, as expressões regulares podem ser uma forma eficiente e elegante de realizar essas tarefas.

## Como fazer:

Para utilizar expressões regulares em Kotlin, você deve importar a biblioteca `Regex` e usar seus métodos para criar e manipular padrões de texto.

```Kotlin
import kotlin.text.Regex

val regex = Regex("Kotlin") // cria um padrão que irá buscar a palavra "Kotlin" em uma string
val str = "A linguagem Kotlin é incrível"

val match = regex.find(str) // encontra a primeira ocorrência do padrão em `str`
println(match) // imprime "RegEx("Kotlin")"

val replaceStr = regex.replace(str, "Java") // substitui o padrão encontrado por "Java"
println(replaceStr) // imprime "A linguagem Java é incrível"
```

## Aprofundando:

Além de buscar e substituir padrões, as expressões regulares também possuem diversos outros recursos em Kotlin. É possível usar quantificadores para especificar a quantidade de ocorrências desejadas, ou classes de caracteres como `\d` para encontrar dígitos ou `\w` para encontrar caracteres alfanuméricos.

Também é possível utilizar expressões regulares para validar formatos de strings, como emails, telefones e documentos, garantindo que eles estejam em conformidade com a formatação esperada.

Embora possa parecer complicado à primeira vista, dominar o uso de expressões regulares em Kotlin pode economizar muito tempo e esforço em aplicações que lidam com manipulação de texto.

## Veja também:

- Documentação oficial de Kotlin: https://kotlinlang.org/docs/reference/regular-expressions.html
- Tutorial de expressões regulares em Kotlin: https://www.raywenderlich.com/23016533-regex-in-kotlin-using-regular-expressions