---
title:                "Kotlin: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Porque escrever um arquivo de texto?

Escrever um arquivo de texto é uma habilidade importante para qualquer programador Kotlin. Isso permite que você armazene e manipule dados de uma forma organizada e acessível. Também pode ser usado para criar arquivos de log, salvar configurações ou até mesmo gerar relatórios.

## Como fazer?

Comece importando o pacote `java.io` para a sua classe. Em seguida, você pode criar o objeto `File` com o caminho e nome do arquivo que deseja escrever. Por exemplo: 

```Kotlin
import java.io.*

val file = File("arquivo.txt")
```

Agora você pode usar o método `writeText()` para escrever um conteúdo no arquivo. Aqui está um exemplo simples: 

```Kotlin
file.writeText("Hello world!")
```

Se você quiser adicionar mais texto ao arquivo existente, você pode usar o método `appendText()` em vez de `writeText()`.

## Mergulho Profundo

Para escrever um arquivo de texto de forma mais eficiente, você pode usar a classe `BufferedWriter`. Isso permite que você escreva grandes quantidades de dados em um arquivo sem ter que recarregar o conteúdo do arquivo a cada vez.

Aqui está um exemplo de como usar o `BufferedWriter` para escrever linhas de texto em um arquivo: 

```Kotlin
val writer: BufferedWriter = file.bufferedWriter()
writer.use {
    it.write("Linha 1")
    it.newLine()
    it.write("Linha 2")
    it.newLine()
    it.write("Linha 3")
}
```

## Veja também

- [Documentação oficial do Kotlin sobre escrita de arquivos](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/write-text.html)
- [Tutorial sobre escrita de arquivos de texto em Kotlin](https://www.tutorialkart.com/kotlin/writetext-file-using-kotlin/)
- [Exemplos de uso do BufferedWriter](https://www.programiz.com/kotlin-programming/examples/write-file)