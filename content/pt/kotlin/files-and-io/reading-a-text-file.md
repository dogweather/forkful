---
date: 2024-01-20 17:54:34.988032-07:00
description: "Como Fazer: Sa\xEDda (supondo que `seu_arquivo.txt` cont\xE9m \"Ol\xE1\
  , Kotlin!\")."
lastmod: '2024-04-05T21:53:46.901194-06:00'
model: gpt-4-1106-preview
summary: "Sa\xEDda (supondo que `seu_arquivo.txt` cont\xE9m \"Ol\xE1, Kotlin!\")."
title: Lendo um arquivo de texto
weight: 22
---

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
