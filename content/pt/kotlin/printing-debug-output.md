---
title:                "Kotlin: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por Que

Você já se encontrou em uma situação em que seu código não estava funcionando corretamente e você não conseguia descobrir o porquê? Muitas vezes, pode ser difícil determinar onde exatamente o problema está ocorrendo. É por isso que imprimir saída de debug pode ser uma ferramenta útil para os programadores.

## Como Fazer

Para imprimir saída de debug em Kotlin, você pode usar a função `println()` ou a função `Log.d()` da biblioteca de Log do Android. Aqui está um exemplo simples de como usar essas funções:

```Kotlin
val nome = "Rafaela"
val idade = 25
println("O nome é $nome e a idade é $idade.")
```

Saída:
```
O nome é Rafaela e a idade é 25.
```

Você também pode usar as funções de impressão de debug em expressões lógicas para ver os valores das variáveis enquanto seu código está sendo executado. Veja um exemplo:

```Kotlin
val numero1 = 10
val numero2 = 5

println("A soma de $numero1 e $numero2 é ${numero1 + numero2}.")
```

Saída:
```
A soma de 10 e 5 é 15.
```

## Aprofundando

A impressão de debug é especialmente útil durante o processo de depuração de código, que é o processo de encontrar e corrigir erros em seu código. Ao imprimir a saída de valores de variáveis ​​e expressões, você pode ver como eles estão sendo manipulados e comparar com o resultado esperado. Isso pode ajudar a identificar possíveis problemas em seu código.

Além disso, você pode usar a impressão de debug para entender melhor o fluxo de execução do seu código. Ao imprimir valores dentro de loops e condicionais, você pode ver como seu código está se comportando em diferentes cenários.

Uma dica para imprimir saída de debug efetivamente é usar informações descritivas, como o nome da variável ou uma descrição do que o valor representa. Isso pode ajudar a identificar a qual parte do código pertence a saída de debug específica.

## Ver Também

- [Documentação da Função `println()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html)
- [Documentação da Função `Log.d()`](https://developer.android.com/reference/android/util/Log.html#d(java.lang.String,%20java.lang.String))