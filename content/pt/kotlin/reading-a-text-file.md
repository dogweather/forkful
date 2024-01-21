---
title:                "Lendo um arquivo de texto"
date:                  2024-01-20T17:54:34.988032-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Ler um arquivo de texto significa extrair a informação dele para ser processada. Programadores fazem isso para manipular dados, configurar programas ou simplesmente exibir o conteúdo na tela.

## Como Fazer:
```kotlin
import java.io.File

fun main() {
    val filePath = "seu_arquivo.txt"
    val fileContent = File(filePath).readText()
    println(fileContent)
}
```
Saída (supondo que `seu_arquivo.txt` contém "Olá, Kotlin!"):
```
Olá, Kotlin!
```

Outro exemplo com leitura linha a linha:
```kotlin
fun main() {
    val filePath = "seu_arquivo.txt"
    File(filePath).forEachLine { linha ->
        println(linha)
    }
}
```

## Aprofundando
Antigamente, ler um arquivo em linguagens de programação podia ser complicado, exigindo várias linhas de código para tratar erros e garantir que os recursos fossem fechados corretamente. Em Kotlin, esse processo é simplificado com funções de extensão que cuidam da maior parte do trabalho sujo.

Alternativas incluem `bufferedReader()` para arquivos grandes, pois lê o texto de forma mais eficiente:
```kotlin
File("seu_arquivo.txt").bufferedReader().use { reader ->
    reader.lineSequence().forEach { linha ->
        println(linha)
    }
}
```

Detalhes de implementação: Kotlin faz uso das facilidades da Java Standard Library. Por isso, ler arquivos em Kotlin geralmente envolve chamar métodos de `java.io.File`. Há também `java.nio.file.Files` para maior controle sobre a leitura.

## Veja Também
- [Tutorial Java - Reading, Writing, and Creating Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)