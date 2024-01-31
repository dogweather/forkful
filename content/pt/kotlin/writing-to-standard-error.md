---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que É & Por Que?
Escrever no erro padrão (standard error) em Kotlin permite separar mensagens de erro das saídas normais do programa. Programadores fazem isso para diagnosticar problemas e facilitar a depuração sem misturar com saídas regulares.

## Como Fazer:
```kotlin
fun main() {
    println("Saida padrão")
    System.err.println("Erro padrão")
}
```
Resultado:
```
Saida padrão
Erro padrão
```
A mensagem "Erro padrão" será escrita em `System.err` em vez de `System.out`.

## Mergulho Profundo:
Em sistemas UNIX, desde as décadas de 60 e 70, existe a convenção de ter saídas separadas para dados regulares (`stdout`) e erros (`stderr`). No Kotlin, `System.err` é um `PrintStream` que por padrão escreve na saída de erro do seu ambiente de execução. Alternativas incluem o uso de logging frameworks como Log4j, que fornecem uma gestão mais avançada das mensagens de erro e outras informações. Kotlin adere ao padrão JVM e usa `System.err` diretamente para escrever em `stderr`; isso pode ser redirecionado e manipulado conforme necessário.

## Veja Também:
- Para saber mais sobre logging com Log4j em Kotlin: [https://logging.apache.org/log4j/kotlin/](https://logging.apache.org/log4j/kotlin/)
