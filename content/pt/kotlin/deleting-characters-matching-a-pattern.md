---
title:                "Kotlin: Excluindo caracteres que correspondem a um padrão."
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, precisamos fazer alterações em strings e isso pode incluir excluir caracteres que correspondam a um determinado padrão. Neste artigo, vamos explorar o processo de exclusão de caracteres que correspondem a um padrão usando Kotlin. Este processo pode ser útil para limpar e manipular dados de strings em um programa.

## Como fazer

Para excluir caracteres de uma string em Kotlin, podemos usar o método `replace()` com uma expressão regular para especificar o padrão. Vamos dar uma olhada em um exemplo simples:

```Kotlin
fun main() {
    val string = "Olá, mundo! #Programação #Kotlin"
    val novaString = string.replace(Regex("[!#]"), "")
    println(novaString)
}
```

Neste exemplo, usamos a função `replace()` para excluir os caracteres "!" e "#". A expressão regular `[!#]` significa que devemos substituir qualquer ocorrência desses caracteres por uma string vazia, o que resulta em "Olá, mundo Programação Kotlin" sendo impresso no console.

O método `replace()` também pode ser usado para substituir um padrão por outro. Por exemplo, se quisermos substituir todos os caracteres de vogais por "*" em uma string, podemos fazer o seguinte:

```Kotlin
fun main() {
    val string = "Essa é uma frase com vogais"
    val novaString = string.replace(Regex("[aeiou]"), "*")
    println(novaString)
}
```

Isso resultará em "*ss* é *m* fr*s* c*m v*g**s" sendo impresso no console.

## Deep Dive

Agora, vamos dar uma olhada mais profunda em como a exclusão de caracteres funciona em Kotlin. O método `replace()` usa uma expressão regular (Regex) para especificar o padrão que deve ser correspondido e excluído. Uma expressão regular é uma sequência de caracteres que define um padrão de texto. É uma ferramenta poderosa para manipular dados em strings e pode ser usada em diversas linguagens de programação.

Existem várias possibilidades para construir expressões regulares em Kotlin. Você pode usar metacaracteres, como "." ou "*", para especificar padrões mais complexos. Também é possível usar o caractere "^" para indicar que o padrão deve ser encontrado apenas no início da string, ou o caractere "$" para indicar que o padrão deve ser encontrado apenas no final da string.

## Veja também

- [Documentação Oficial do Kotlin para `replace()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Tutorial sobre Expressões Regulares em Kotlin](https://www.baeldung.com/kotlin-regular-expressions)
- [Interactive Kotlin Tutorial](https://play.kotlinlang.org/learn/welcome/overview)