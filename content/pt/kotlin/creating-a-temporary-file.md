---
title:                "Kotlin: Criando um arquivo temporário"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Kotlin?

Criar arquivos temporários é uma prática comum em linguagens de programação para armazenar informações temporariamente. Em Kotlin, também é possível criar arquivos temporários facilmente para fins de teste ou salvar dados temporários.

## Como criar um arquivo temporário em Kotlin

Para criar um arquivo temporário em Kotlin, basta seguir os seguintes passos:

1. Importar o pacote `java.io.File` no início do seu arquivo Kotlin.
2. Usar a função `createTempFile()` do objeto `File` para criar o arquivo temporário.

Exemplo de código:

```Kotlin
import java.io.File

val tempFile = File.createTempFile("temp", ".txt")
```

O código acima irá criar um arquivo temporário com o nome "temp" e a extensão ".txt". Você também pode especificar o diretório onde o arquivo será criado, passando-o como parâmetro na função `createTempFile()`.

Após criar o arquivo temporário, você pode utilizar métodos como `writeText()` ou `appendText()` para escrever informações no arquivo. Para ler o conteúdo do arquivo, você pode utilizar o método `readText()`.

Exemplo de uso:

```Kotlin
tempFile.writeText("Este é um arquivo temporário.")

println(tempFile.readText()) // Output: "Este é um arquivo temporário."
```

## Aprofundando-se em criar arquivos temporários em Kotlin

Além da função `createTempFile()`, o objeto `File` também possui outras funções úteis para trabalhar com arquivos temporários, como `deleteOnExit()` que permite que o arquivo seja excluído automaticamente quando o programa é encerrado, e `createTempDirectory()` que cria um diretório temporário em vez de um arquivo.

É importante lembrar que arquivos temporários são apagados automaticamente após o encerramento do programa. Se você precisa armazenar informações permanentemente, é recomendado utilizar outros métodos, como criar arquivos regulares.

## Veja também

- Documentação oficial do Kotlin sobre arquivos temporários: 
https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html#com.google.common.io.Files
- Tutorial sobre como criar arquivos em Kotlin: https://www.programiz.com/kotlin-programming/file-handling
- Exemplo de uso de arquivos temporários em projetos de teste: https://stackoverflow.com/questions/35136941/how-do-i-create-a-temp-file-for-unit-testing-in-kotlin