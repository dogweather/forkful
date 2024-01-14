---
title:                "Kotlin: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Kotlin?

O processo de criação de arquivos temporários é muito importante em linguagens de programação, incluindo o Kotlin. Esses arquivos são úteis para armazenar informações temporárias durante a execução de um programa, economizando espaço de armazenamento e evitando conflitos com outros arquivos existentes. Além disso, eles são excluídos automaticamente após o término do programa, o que garante uma melhor organização dos dados.

## Como criar um arquivo temporário em Kotlin

Para criar um arquivo temporário em Kotlin, podemos usar a classe `java.io.File` e seus métodos `createTempFile()` e `deleteOnExit()`. O primeiro método é responsável por criar o arquivo temporário e o segundo garante que ele será excluído após o término do programa.

```Kotlin 
val tempFile = File.createTempFile("temp", ".txt")
tempFile.deleteOnExit()
```

O código acima criará um arquivo chamado "temp" com a extensão ".txt". Podemos usar o arquivo criado para armazenar informações temporárias durante a execução do programa.

## Deep Dive

Além de usar a classe `java.io.File`, também podemos criar arquivos temporários usando a classe `kotlin.io.Files` do pacote padrão do Kotlin. Esta classe oferece métodos como `createTempFile()` e `createTempDirectory()` que facilitam a criação de arquivos temporários.

Além disso, também podemos personalizar o local onde o arquivo temporário será criado, especificando o diretório como parâmetro no método `createTempFile()`. Isso nos dá um melhor controle sobre a localização dos nossos arquivos temporários.

## Veja também

- [Documentação oficial do Kotlin sobre a classe `java.io.File` ](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Exemplo de criação de arquivo temporário em Kotlin](https://www.baeldung.com/kotlin-temporary-file)
- [Tutorial sobre arquivos temporários em Kotlin](https://www.tutorialkart.com/kotlin/create-temporary-file-kotlin-with-examples/)