---
title:                "Imprimindo saída de depuração"
html_title:           "Kotlin: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que e Por que?

Frequentemente, durante o processo de escrita de código, os programadores precisam verificar se as variáveis estão contendo os valores esperados para garantir que o código esteja funcionando corretamente. Para fazer isso, eles adicionam declarações de impressão de saída ao código, conhecidas como saída de depuração. Isso é especialmente útil durante a depuração de erros ou quando se está trabalhando com log de informações.

## Como fazer:

No Kotlin, existem duas maneiras principais de imprimir saída de depuração: usando `println()` e `Log.d()`.

```
Kotlin println("Hello World!") //imprime "Hello World!" na saída padrão

```

```
Kotlin val name = "Maria"
Log.d("Debug", "Name is: $name") // imprime "Name is: Maria" nos logs do Android Studio
```

## Tudo Sobre:

Saída de depuração é uma técnica que vem sendo usada há muito tempo para diagnosticar problemas em código. No passado, os programadores costumavam adicionar declarações `print` ou `printf` ao código para imprimir valores de variáveis. No entanto, com o surgimento de ferramentas de depuração, essa técnica tornou-se menos utilizada.

Além disso, existem outras formas de depuração, como o uso de breakpoints ou ferramentas de inspeção de variáveis, que fornecem um método mais eficiente para encontrar e corrigir erros no código.

Na implementação em Kotlin, a função `println()` é basicamente uma chamada ao método `toString()` do objeto passado como argumento. Já o método `Log.d()` é usado para imprimir mensagens de depuração específicas da plataforma Android.

## Veja também:

- Documentação oficial do Kotlin: https://kotlinlang.org/docs/reference/
- Como usar breakpoints no Android Studio: https://developer.android.com/studio/debug/
- Kotlin Logging: https://ktor.io/quickstart/guide/generate-logging.html