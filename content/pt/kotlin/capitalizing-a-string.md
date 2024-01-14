---
title:    "Kotlin: Maiúscula de uma string"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é um processo muito comum na programação. Isso envolve transformar a primeira letra de cada palavra em maiúscula e o restante das letras em minúscula. Existem várias razões pelas quais alguém pode querer capitalizar uma string em Kotlin. Pode ser para melhorar a legibilidade do código, para formatar dados de entrada ou para atender a requisitos específicos de um projeto. Neste artigo, você aprenderá como capitalizar uma string em Kotlin e também se aprofundará no processo para entender melhor como funciona.

## Como fazer em Kotlin

Capitalizar uma string é um processo simples em Kotlin e pode ser facilmente realizado usando o método `capitalize()` da classe `String`. Aqui está um exemplo de código:

```Kotlin
val str = "kotlin é incrível"
val capitalizedStr = str.capitalize()

println(capitalizedStr)

// output: Kotlin é incrível
```

Como você pode ver, o método `capitalize()` mudou a letra "k" para maiúscula e o restante da string permaneceu inalterado. Isso pode ser feito com qualquer string, independentemente do número de palavras que ela contém.

Além do `capitalize()`, há também o método `toUpperCase()` que transforma todas as letras de uma string em maiúsculas. Aqui está um exemplo:

```Kotlin
val str = "programação kotlin"
val upperCaseStr = str.toUpperCase()

println(upperCaseStr)

// output: PROGRAMAÇÃO KOTLIN
```

Por outro lado, se você quiser que todas as letras sejam em minúsculas, pode usar o método `toLowerCase()`. Veja:

```Kotlin
val str = "STRING EM CAIXA ALTA"
val lowerCaseStr = str.toLowerCase()

println(lowerCaseStr)

// output: string em caixa alta
```

## Profundidade no processo de capitalização de strings

Agora que você aprendeu como capitalizar uma string em Kotlin, é hora de entender um pouco mais sobre o processo em si. O método `capitalize()` apenas muda a primeira letra para maiúscula. Isso significa que o restante das letras permanecerá inalterado, mesmo que já estivessem em maiúsculas. Além disso, se uma palavra começar com um caractere especial ou número, ela continuará inalterada. Veja alguns exemplos:

```Kotlin
val str1 = "123abc"
val capitalizedStr1 = str1.capitalize()

println(capitalizedStr1)

// output: 123abc

val str2 = "#kotlin"
val capitalizedStr2 = str2.capitalize()

println(capitalizedStr2)

// output: #kotlin
```

Portanto, é importante ter cuidado ao usar o método `capitalize()` e garantir que esteja atendendo às suas necessidades.

## Veja também

- [Documentação oficial do método capitalize em Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Como capitalizar uma string em Java](https://www.baeldung.com/java-string-capitalize)
- [Outras funções de string em Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_string_operations.htm)