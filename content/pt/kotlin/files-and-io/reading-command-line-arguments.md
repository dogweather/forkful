---
date: 2024-01-20 17:56:27.077350-07:00
description: "Como fazer: Sa\xEDda, se for chamado com um argumento."
lastmod: '2024-04-05T21:53:46.899287-06:00'
model: gpt-4-1106-preview
summary: "Sa\xEDda, se for chamado com um argumento."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como fazer:
```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Olá, ${args[0]}!")
    } else {
        println("Olá, desconhecido! Por favor, informe seu nome.")
    }
}
```
Saída, se for chamado com um argumento: 
```
Olá, João!
```
Saída, se for chamado sem argumentos:
```
Olá, desconhecido! Por favor, informe seu nome.
```

## Aprofundando:
Historicamente, acessar argumentos da linha de comando é uma prática desde os primórdios da computação, quando as interfaces gráficas ainda não existiam. No Kotlin, é muito semelhante ao que se faz em outras linguagens da JVM, como o Java: os argumentos são passados para o método 'main' como um array de Strings.

Alternativas ao uso de argumentos da linha de comando incluem a interação com o usuário via interfaces gráficas ou arquivos de configuração, mas essas são instruções adicionais que podem tornar a execução menos direta.

Quanto aos detalhes da implementação, em Kotlin, desde que você defina seu array de Strings como 'args' no método 'main', você pode acessar qualquer argumento passado pelo índice. Por exemplo, `args[0]` é o primeiro argumento. É importante sempre verificar se a array não está vazia antes de tentar acessá-la para evitar exceções de índice fora dos limites.

## Veja também:
- Documentação oficial do Kotlin sobre funções, incluindo o 'main': https://kotlinlang.org/docs/functions.html#main-function
- Kotlin docs on array types (Tipos de arrays no Kotlin): https://kotlinlang.org/docs/basic-types.html#arrays
- Uma explicação sobre argumentos da linha de comando no Java - útil para compreender as similaridades com Kotlin: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html
