---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:48:00.065528-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Medir o tamanho de uma string é descobrir quantos caracteres ela tem. Programadores fazem isso para validar entradas, manipular textos ou simplesmente porque o comprimento de uma string é crucial para a lógica do código.

## Como Fazer:
```kotlin
fun main() {
    val greeting = "Olá, Mundo!"
    println("O comprimento da string é: ${greeting.length}")
}

// Saída: O comprimento da string é: 12
```

Uma operação direta: use `length` para pegar o tamanho.

Quer lidar com strings nulas com segurança? Assim:

```kotlin
fun main() {
    val nullableGreeting: String? = null
    println("O comprimento da string é: ${nullableGreeting?.length ?: "String nula"}")
}

// Saída: O comprimento da string é: String nula
```

## Mergulho Profundo
Historicamente, contar caracteres parece trivial, mas é uma parte fundamental da computação. Em algumas linguagens de programação antigas ou de baixo nível, você teria que percorrer uma string manualmente para contar os caracteres.

Alternativas? Em Kotlin, `length` é o jeito de ir porque é uma propriedade da classe String, mas você pode também querer manipular strings de outras formas.

Por exemplo, se você está trabalhando com texto que poderia ter caracteres Unicode complexos, você poderá explorar a normatização e outras técnicas para lidar com grupos de caracteres compostos.

Quanto aos detalhes de implementação, Kotlin roda em uma máquina virtual de Java, então `length` na verdade chama o método equivalente na classe `String` do Java.

## Veja Também
- Documentação oficial Kotlin para Strings: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Kotlin e Unicode: [https://kotlinlang.org/docs/whatsnew1520.html#better-handling-of-unicode-in-regex-patterns](https://kotlinlang.org/docs/whatsnew1520.html#better-handling-of-unicode-in-regex-patterns)