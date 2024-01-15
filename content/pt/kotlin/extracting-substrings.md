---
title:                "Extraindo subcadeias de caracteres"
html_title:           "Kotlin: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extração de substrings é um processo importante na programação, pois permite que você obtenha partes específicas de uma string. Isso pode ser útil em diversas situações, como manipulação de dados, formatação de texto, entre outros.

## Como fazer

Abaixo, você encontrará dois exemplos de como extrair substrings em Kotlin:

#### Exemplo 1:

```Kotlin
val texto = "Olá mundo!"
val substring = texto.substring(4)
println(substring) 
```

**Saída:** "mundo!"

Neste exemplo, usamos o método `substring()` para extrair uma parte da string original a partir do índice 4.

#### Exemplo 2:

```Kotlin
val texto = "Kotlin é uma linguagem de programação moderna"
val substring = texto.substring(0, 13)
println(substring)
```

**Saída:** "Kotlin é uma"

Neste caso, especificamos o início e o fim da substring que queremos extrair. No exemplo acima, utilizamos os índices 0 e 13, que correspondem ao começo e ao fim da palavra "Kotlin".

## Mergulho profundo

O método `substring()` aceita dois parâmetros: o índice de início e o índice de fim. O índice de início é inclusivo, ou seja, o caractere correspondente a esse índice também será incluído na substring resultante. Já o índice de fim é exclusivo, ou seja, o caractere correspondente a esse índice não será incluído.

Além disso, também é possível utilizar o método `substringAfter()` e `substringBefore()` para extrair substrings a partir de um determinado caractere ou texto. Por exemplo:

```Kotlin
val texto = "Eu amo programar em Kotlin!"
val substring = texto.substringAfter("amo")
println(substring)
```

**Saída:** " programar em Kotlin!"

Neste exemplo, utilizamos o método `substringAfter()` para extrair a substring que vem após a palavra "amo".

## Veja também

- [Documentação oficial do método substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Outros métodos de manipulação de strings em Kotlin](https://kotlinlang.org/docs/basic-syntax.html#string-templates)