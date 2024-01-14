---
title:    "Kotlin: Encontrando o comprimento de uma string"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa básica, mas importante, na programação. Saber o tamanho de uma string pode ser útil para várias tarefas, como validar entrada do usuário, manipular dados ou formatar saídas. Neste artigo, vamos explorar como encontrar o comprimento de uma string em Kotlin.

## Como fazer

Em Kotlin, podemos encontrar o comprimento de uma string usando o método `length()`. Veja o exemplo abaixo:

```Kotlin
val string = "Olá, mundo!"
val comprimento = string.length()

println(comprimento) // 12
```

Como podemos ver, o método `length()` retorna o número de caracteres na string, incluindo espaços e pontuação.

Podemos também usar o operador `.` para acessar o método diretamente da string, como mostrado a seguir:

```Kotlin
val comprimento = "Olá, mundo!".length

println(comprimento) // 12
```

## Mergulho Profundo

Sabemos que uma string é uma sequência de caracteres, mas como o tamanho de uma string é realmente medido? Em Kotlin, uma string é representada internamente por um array de caracteres. Portanto, o comprimento de uma string é simplesmente o tamanho desse array.

Além disso, o método `length()` é otimizado para ser executado em tempo constante. Isso significa que não importa quanto tempo a string tenha, o tempo de execução sempre será o mesmo. Isso torna o método eficiente para a maioria dos casos de uso.

## Veja também

Aqui estão alguns links para mais informações sobre strings e o método `length()` em Kotlin:

- Documentação oficial sobre strings em Kotlin: https://kotlinlang.org/docs/basic-types.html#strings
- Outras operações úteis em strings: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/
- Exemplos de aplicação do método `length()`: https://www.tutorialspoint.com/kotlin/kotlin_string_length.htm