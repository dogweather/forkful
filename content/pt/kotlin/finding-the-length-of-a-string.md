---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Encontrar o comprimento de uma string significa determinar o número de caracteres que ela contém. Essa é uma tarefa comum na programação, usada para validar entradas de dados, gerenciar a apresentação do texto na interface do usuário e muito mais.

## Como fazer

Em Kotlin, você pode encontrar o comprimento de uma string usando a propriedade `length`:

```kotlin
val string = "Olá, mundo!"
println("Comprimento da string: ${string.length}")
```

A saída será:

```
Comprimento da string: 12
```

## Deep Dive

A propriedade `length` em Kotlin é inteiramente tirada de Java, a linguagem em que Kotlin foi construído. Esta operação é eficiente, porque as strings em Kotlin (e em Java) são imutáveis e armazenam seu comprimento como um campo.

Uma alternativa para encontrar o comprimento de uma string poderia ser iterar sobre cada caractere na string e contar os caracteres. No entanto, isso é desnecessariamente complexo e menos eficiente do que usar a propriedade `length`.

A implementação detalhada da propriedade `length` em Kotlin é simplesmente retornar o valor armazenado do campo `length` da string original.

## Ver tambem

Para obter mais detalhes sobre strings e suas propriedades e funções em Kotlin, você pode verificar estas fontes:

- Kotlin String Class - Documentação oficial do Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)

- Kotlin for Java Developers: Comparison to Java Programming Language: [https://kotlinlang.org/docs/reference/comparison-to-java.html](https://kotlinlang.org/docs/reference/comparison-to-java.html)