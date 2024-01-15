---
title:                "Criando um arquivo temporário"
html_title:           "Kotlin: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário?

Criar um arquivo temporário é útil quando precisamos armazenar temporariamente dados que não serão mais necessários em um momento posterior. Isso é especialmente útil quando estamos lidando com grandes quantidades de informações e queremos manter nosso código limpo e organizado.

## Como fazer:

```Kotlin
import java.io.File

fun main() {
  val tempFile = File.createTempFile("temp", ".txt")
  tempFile.writeText("Este é um exemplo de arquivo temporário!")
}
```

Ao executar o código acima, um arquivo temporário será criado na pasta padrão do sistema operacional com o nome "temp12345.txt". O conteúdo do arquivo será "Este é um exemplo de arquivo temporário!". Você também pode especificar o diretório onde deseja criar o arquivo temporário, passando-o como um parâmetro para a função `createTempFile()`.

## Deep Dive:

Criar um arquivo temporário é uma tarefa simples em Kotlin, pois a linguagem já possui uma classe integrada para lidar com isso: `java.io.File`. Além disso, ao usar o método `createTempFile()`, o arquivo será automaticamente excluído quando o programa for encerrado.

No método `createTempFile()`, o primeiro parâmetro é o prefixo do arquivo e o segundo é sua extensão. Você também pode usar `createTempFile(prefix, suffix, directory)` para especificar um diretório diferente para armazenar seu arquivo temporário.

## Veja também:

- Documentação oficial do `java.io.File` em Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html
- Tutorial sobre manipulação de arquivos em Kotlin: https://www.raywenderlich.com/1068424-kotlin-tutorial-for-android-getting-started
- Artigo sobre as vantagens de usar Kotlin para desenvolvimento de aplicações: https://medium.com/estudiodigital/por-que-usar-kotlin-para-desenvolvimento-de-aplica%C3%A7%C3%B5es-espaciais-6105f274e59e