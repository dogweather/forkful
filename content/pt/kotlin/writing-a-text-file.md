---
title:                "Escrevendo um arquivo de texto"
html_title:           "Kotlin: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos?
Escrever em um arquivo de texto significa salvar informações em um formato legível por humanos, como palavras, números ou símbolos, em um documento de texto. Os programadores geralmente escrevem em arquivos de texto para armazenar e recuperar dados de maneira eficiente e organizada em seus programas.

## Como fazer:
Um arquivo de texto pode ser criado usando a função "writeText" que aceita uma string e o nome do arquivo como parâmetros. Veja um exemplo abaixo:
```Kotlin
fun main() {
    val texto = "Olá, mundo!"
    writeText("arquivo.txt", texto)
}
```
A saída esperada será um arquivo chamado "arquivo.txt" contendo o texto "Olá, mundo!".

## Mergulho profundo:
Esta prática de salvar dados em arquivos de texto acompanha o desenvolvimento da computação desde seus primórdios. Hoje, existem diversas alternativas, como bancos de dados e linguagens de marcação, mas os arquivos de texto ainda são amplamente utilizados por sua simplicidade e compatibilidade com várias linguagens de programação. Além disso, ao escrever em um arquivo de texto, também é possível controlar manualmente a formatação e a organização dos dados.

## Veja também:
Para mais informações sobre como escrever em arquivos de texto em Kotlin, confira a documentação oficial: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/write-text.html