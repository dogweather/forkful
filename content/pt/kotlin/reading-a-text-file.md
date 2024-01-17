---
title:                "Leitura de um arquivo de texto"
html_title:           "Kotlin: Leitura de um arquivo de texto"
simple_title:         "Leitura de um arquivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que é e por que? 
Ler um arquivo de texto é simplesmente ler o conteúdo de um arquivo que contém texto, como um arquivo .txt ou .csv. Os programadores geralmente precisam ler arquivos de texto para obter dados ou configurar um programa.

## Como fazer:
O processo básico para ler um arquivo de texto em Kotlin é o seguinte:
```
val arquivo = File("nome_do_arquivo.txt")
val linhas = arquivo.readLines()
for (linha in linhas) {
    println(linha)
}
```

Isso cria um objeto `File` que aponta para o arquivo desejado, e então lê todas as linhas do arquivo usando o método `readLines()`. Em seguida, percorremos as linhas com um loop e as imprimimos no console.

## Deep Dive:
Ler arquivos de texto é uma tarefa comum na programação e é feita com muita frequência. Existem várias formas de ler arquivos de texto em Kotlin, como usando `BufferedReader` ou `InputStream`. Além disso, existem bibliotecas externas, como o Apache Commons IO, que oferecem funcionalidades mais avançadas para leitura de arquivos de texto.

Uma coisa a ter em mente é a codificação do arquivo de texto. Dependendo do idioma e da configuração do sistema, pode ser necessário especificar a codificação ao ler o arquivo para garantir que os caracteres sejam lidos corretamente.

## Veja também:
- Documentação oficial do Kotlin sobre leitura de arquivos: https://kotlinlang.org/docs/tutorials/kotlin-for-py/read-write-files.html
- Tutorial de vídeo sobre leitura de arquivos em Kotlin: https://www.youtube.com/watch?v=HBSHdI2IZ3A
- Biblioteca Apache Commons IO: https://commons.apache.org/proper/commons-io/index.html