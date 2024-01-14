---
title:                "Kotlin: Imprimindo saída de depuração"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração em Kotlin?

Imprimir saída de depuração é uma ferramenta útil para os desenvolvedores Kotlin entenderem o fluxo de seus códigos e identificarem possíveis erros. Ajuda a visualizar os valores das variáveis ​​em diferentes pontos do código, permitindo uma depuração mais eficiente.

## Como Fazer

Para imprimir saída de depuração em Kotlin, podemos utilizar a função print() ou println(). A função print() imprime no console sem uma nova linha, enquanto a função println() imprime com uma nova linha. Podemos utilizar essas funções passando as variáveis ou strings desejadas como parâmetros.

```Kotlin
val nome = "João"
print("Seu nome é: ")
print(nome)

// Saída: Seu nome é: João
```

Podemos ainda formatar a saída de depuração usando a função String.format() ou expressões de formatação. Por exemplo:

```Kotlin
val numero = 10
println("O número é: $numero")

// Saída: O número é: 10
```

Também podemos utilizar a função Any?.toString() para converter qualquer tipo de dado em uma string para ser impressa na saída de depuração.

## Deep Dive

Além das funções print() e println(), existem outras formas de imprimir saída de depuração em Kotlin. Uma delas é utilizando a biblioteca de Log que está incluída no Android Studio. Essa biblioteca permite que registremos mensagens de log com diferentes níveis de gravidade, como debug, informação, aviso e erro. Isso permite uma depuração mais detalhada e organizada.

Outra forma é utilizar a biblioteca de depuração integrada do Kotlin, que permite que coloquemos pontos de interrupção no código e visualizemos o valor das variáveis no momento da execução. Isso é especialmente útil em situações de bugs que não podem ser facilmente identificados apenas com a saída de depuração no console.

## Veja Também

- [Documentação Oficial do Kotlin sobre Depuração](https://kotlinlang.org/docs/debugging.html)
- [Guia do Jetbrains sobre Depuração em Kotlin](https://www.jetbrains.com/help/kotlin/debugging.html)
- [Utilizando Log para saída de depuração em Kotlin no Android](https://developer.android.com/reference/android/util/Log)