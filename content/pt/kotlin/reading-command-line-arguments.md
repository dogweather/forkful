---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que & Por quê?

A leitura de argumentos de linha de comando é uma maneira de um programa interagir com o usuário, permitindo que eles passem informações diretamente no momento da execução. Os programadores fazem isso para permitir a customização do programa em tempo de execução, sem ter que alterar o código.

## Como fazer:

Para ler argumentos de linha de comando em Kotlin, usamos o array `args` em `fun main()`. Vamos dar uma olhada em um exemplo simples:

```Kotlin
fun main(args: Array<String>) {

    // Se nenhum argumento, exibir uma mensagem
    if (args.isEmpty()) {
        println("Por favor, insira argumentos")
        return
    }

    println("Argumentos fornecidos:")
    args.forEach {
        println(it)
    }
}
```
Execute o código acima com argumentos de linha de comando e veja os resultados:
`$ kotlinc Main.kt -include-runtime -d Main.jar && java -jar Main.jar argumento1 argumento2`
Output:
```
Argumentos fornecidos:
argumento1
argumento2
```

## Mergulhe a fundo:

Historicamente, a leitura de argumentos de linha de comando é uma prática comum em linguagens de programação desde que os primeiros sistemas operacionais com linha de comando surgiram. É uma maneira prática de fornecer opções ao executar um programa.

Existem vários métodos alternativos para fornecer dados a um programa, incluindo o uso de arquivos de configuração ou variáveis de ambiente. No entanto, os argumentos de linha de comando geralmente são a melhor opção para a personalização em tempo de execução do comportamento do programa.

Em termos de implementação interna, quando um programa Kotlin é iniciado, o sistema operacional passa os argumentos de linha de comando para o programa como uma matriz de strings. Esta matriz é então passada para a função `main()`, permitindo que o programa acesse as informações.

## Veja também:

Para mais informações sobre argumentos de linha de comando em Kotlin e em programação em geral, consulte os recursos a seguir:

- Documentação oficial do Kotlin: [https://kotlinlang.org/docs/tutorials/command-line.html](https://kotlinlang.org/docs/tutorials/command-line.html)
- Para uma análise mais profunda de argumentos de linha de comando em programação: [https://www.tutorialspoint.com/programmingmethodology/programming_invocation_arguments.htm](https://www.tutorialspoint.com/programmingmethodology/programming_invocation_arguments.htm)