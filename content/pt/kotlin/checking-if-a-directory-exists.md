---
title:                "Verificando se um diretório existe"
html_title:           "Kotlin: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar se um diretório existe se refere a conferir se um determinado caminho de arquivo aponta para um diretório ou localização que realmente exista no sistema. Os programadores fazem isso para evitar erros de arquivos ausentes, que podem fazer com que o programa falhe ou se comporte de forma imprevisível.

## Como fazer:
Aqui estão alguns exemplos de como verificar se um diretório existe usando a linguagem de programação Kotlin:

Primeiro, importe o pacote necessário:

```Kotlin
import java.nio.file.*
```

Em seguida, você pode utilizar a função `Files.exists()` para verificar se um diretório já existe:

```kotlin
val diretorio = Paths.get("/caminho/para/o/diretorio")
if(Files.exists(diretorio)){
    println("O diretório existe.")
} else {
    println("O diretório não existe.")
}
```

Se o diretório existir, o output será `O diretório existe`. Caso contrário, o output será `O diretório não existe`.

## Mergulhando Mais a Fundo
Até o Java 7, a verificação da existência de um diretório era feita usando o método `File.exists()`. No entanto, com a introdução do pacote `java.nio.file` no Java 7, muitos preferem usar as funções deste pacote, como `Files.exists()`, por serem mais completas e eficientes.

Existem algumas alternativas ao uso do `Files.exists()`. Por exemplo, você pode usar a função `File.isDirectory()` que retornará `true` se o caminho do arquivo existir e for um diretório.

Ao verificar a existência de um diretório em Kotlin, a JVM é a responsável por lidar com os detalhes específicos do sistema operacional, como a padronização dos separadores de caminhos. Portanto, o mesmo código funcionará sem problemas em qualquer sistema operacional que suporte a JVM.

## Veja Também
1. Documentação oficial da classe Files no Kotlin: [Files (Java Platform SE 8 - Oracle docs)](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
2. Documentação oficial da classe Path no Kotlin: [Path (Java Platform SE 8 - Oracle docs)](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html)
3. Documentação oficial da classe File no Kotlin: [File (Java Platform SE 8 - Oracle docs)](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)