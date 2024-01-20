---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que e Porquê?

Ler um arquivo de texto é o processo pelo qual um programa recupera e interpreta informações armazenadas num arquivo texto. Os programadores fazem isso para, por exemplo, processar dados disponíveis em arquivos ou obter configurações armazenadas.

## Como Faz:

Vamos ler um arquivo em Kotlin em cujas linhas contêm números.

```Kotlin 
import java.io.File

fun main() {
    val linhas = File("numeros.txt").readLines()
    linhas.forEach { println(it) }
}
```
Se nosso arquivo contém os números de 1 a 5, cada um numa nova linha:

```output
1
2
3
4
5
```
Isso imprimi cada linha do arquivo.

## Mergulho Profundo:

Historicamente, a leitura de arquivos é uma prática comum em programação desde o início dos tempos dos primeiros computadores. O Kotlin simplifica esse processo, fornecendo várias funções para lidar com a entrada e a saída de arquivos.

Existem alternativas para a função `readLines()`, como `readText()` que lê todo o conteúdo do arquivo numa String, ou `bufferedReader().use { it.readText() }`, que é útil para arquivos grandes, dado que lê o conteúdo sem armazenar tudo na memória.

As funções de leitura de arquivo em Kotlin, como `readLines()`, trabalham internamente com exceções Java. Caso o arquivo não possa ser lido, uma exceção será lançada. Essa implementação facilita a localização e o manuseio de erros.

## Veja Também:

* Documentação oficial do Kotlin em leitura e escrita de arquivos: [link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-lines.html)