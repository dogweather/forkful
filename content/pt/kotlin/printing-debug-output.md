---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Imprimir saídas de depuração é quando os programadores usam códigos para exibir mensagens informativas durante a execução do programa. Isso auxilia na identificação e correção de erros.

## Como fazer:

Para imprimir mensagens de depuração em Kotlin, utilizamos a função `println()`. Simplifica-se assim:
```Kotlin
fun main() {
   val variavel = "Olá, Mundo!"
   println("Mensagem de depuração: $variavel")
}
```
Output:
```
Mensagem de depuração: Olá, Mundo!
```
## Mergulho Profundo:

A atitude de imprimir mensagens de depuração tem uma longa história na programação, desde os primeiros dias do Fortran. Em Kotlin, a função `println()` é a mais comum para esse fim, mas também temos `print()` que funciona de forma semelhante, porém sem a nova linha no final.

Outra alternativa é usar a função `System.out.print()`, mais comum em Java, que também funciona em Kotlin. A última é a biblioteca de logging, como o Log4J ou SLF4J, que provêem mais controle sobre a saída das mensagens.

Embora pareça simples, a função `println()` por debaixo dos panos está na verdade realizando múltiplas operações. Está convertendo o objeto para um string, verificando se é nulo, anexando ao buffer e, finalmente, exibindo no console.

## Veja Também:

Para mais informações sobre depuração e gravação de logs em Kotlin e Java, verifique os seguintes links.

- Depuração no Kotlin: (Link)
- Documentação oficial Kotlin println: (Link)
- Guia de introdução ao SLF4J: (Link)

Lembre-se, a depuração é vital para entender o fluxo do seu código e identificar quaisquer problemas potenciais. Portanto, seja paciente, pratique e continue aprendendo!