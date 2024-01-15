---
title:                "Utilizando expressões regulares"
html_title:           "Kotlin: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

Expressões regulares são uma ferramenta poderosa para manipular strings em qualquer linguagem de programação, incluindo Kotlin. Com elas, podemos encontrar e substituir padrões específicos de texto de maneira eficiente e concisa.

## Como usar expressões regulares em Kotlin

O uso de expressões regulares em Kotlin é bastante simples e é feito através de uma classe chamada `Regex`. Vamos ver alguns exemplos de como podemos criar e utilizar essas expressões:

```Kotlin
// Criando uma expressão regular para encontrar um número de telefone com o formato (xx) xxxxx-xxxx
val regex = Regex("\\(\\d{2}\\)\\s\\d{5}-\\d{4}")
// Verificando se uma string corresponde à expressão regular
val match = regex.matches("(11) 99999-9999")
```

No código acima, utilizamos a barra invertida para escapar os caracteres especiais da expressão regular, como os parênteses e o hífen. Também definimos a quantidade de dígitos em cada parte do número, utilizando o símbolo de repetição `\d{n}`, onde `n` representa o número de dígitos desejado.

Além disso, podemos utilizar a função `find()` para encontrar o primeiro padrão correspondente em uma string, ou a função `findAll()`, que retorna uma lista de todos os padrões encontrados.

```Kotlin
// Encontrando o primeiro padrão correspondente em uma string
val match = regex.find("Tel: (11) 99999-9999")?.value
// Encontrando todos os padrões correspondentes em uma string
val matches = regex.findAll("(11) 99999-9999 e (12) 88888-8888").toList()
```

Também é possível utilizar os símbolos `^` e `$` para indicar o começo e o fim da string, respectivamente. Com eles, podemos criar expressões regulares mais precisas e evitar correspondências indesejadas.

## Profundando nas expressões regulares

As expressões regulares possuem uma grande quantidade de metacaracteres que nos permitem criar padrões complexos de busca. Algumas das principais são:

- `.`: corresponde a qualquer caractere.
- `[]`: corresponde a um conjunto de caracteres. Por exemplo, `[abc]` corresponde a qualquer uma das letras `a`,`b` ou `c`.
- `*`: corresponde a zero ou mais ocorrências do padrão anterior. Por exemplo, `ab*` corresponde a `a`, `ab`, `abb`, `abbb`...
- `+`: corresponde a uma ou mais ocorrências do padrão anterior. Por exemplo, `ab+` corresponde a `ab`, `abb`, `abbb`...
- `?`: corresponde a zero ou uma ocorrência do padrão anterior. Por exemplo, `ab?` corresponde a `a` e `ab`.
- `^`: corresponde ao começo da string.
- `$`: corresponde ao fim da string.
- `|`: corresponde a um padrão ou outro. Por exemplo, `a|b` corresponde a `a` ou `b`.

Podemos utilizar esses metacaracteres em conjunto com os símbolos de repetição para criar expressões regulares mais complexas e precisas.

## Veja também

- [Referência da classe `Regex` em Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Tutorial de Expressões Regulares (em inglês)](https://www.regular-expressions.info/)