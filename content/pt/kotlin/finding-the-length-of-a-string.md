---
title:                "Encontrando o tamanho de uma string"
html_title:           "Kotlin: Encontrando o tamanho de uma string"
simple_title:         "Encontrando o tamanho de uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Aprender a encontrar o comprimento de uma string é essencial para desenvolver habilidades básicas de programação e resolver problemas comuns em projetos. Compreender como determinar o tamanho de uma string pode ser útil em várias situações, desde validar entradas do usuário até formatar saídas de dados.

## Como fazer?

Para encontrar o comprimento de uma string em Kotlin, podemos usar a função `length()` que retorna o número de caracteres na string. Aqui está um exemplo simples:

```Kotlin
val nome = "Aline"
val tamanho = nome.length()
println(tamanho)
```

Este código irá imprimir "5", pois há 5 caracteres na string "Aline". Além disso, podemos usar esta função em conjunto com um loop para encontrar o comprimento de uma string dinamicamente. Por exemplo:

```Kotlin
fun getLength(string: String): Int {
    var count = 0
    for (char in string) {
        count++
    }
    return count
}

val nome = "Maria"
val tamanho = getLength(nome)
println(tamanho)
```

A saída será novamente "5". Neste exemplo, criamos nossa própria função `getLength()` para encontrar o comprimento da string, contando quantas vezes o loop é executado.

## Aprofundando

Embora a função `length()` seja a maneira mais simples de encontrar o comprimento de uma string, existem outras formas de fazer isso. Por exemplo, podemos usar o operador de atribuição de arrays `size()` e até mesmo utilizar o método `substring()` para encontrar a parte da string que queremos e, em seguida, usar a função `length()` nessa substring. Vale a pena explorar e experimentar diferentes abordagens para encontrar o comprimento de uma string.

## Veja também

- [Documentação oficial do Kotlin sobre strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Artigo sobre manipulação de strings em Kotlin](https://www.baeldung.com/kotlin/string-manipulation)