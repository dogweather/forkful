---
title:                "Kotlin: Lendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto pode ser útil para muitas tarefas de programação, desde armazenar e acessar dados até criar relatórios e visualizações usando os dados contidos nos arquivos. Além disso, a leitura de arquivos de texto é uma habilidade essencial para qualquer desenvolvedor de software.

## Como ler um arquivo de texto em Kotlin

Para ler um arquivo de texto em Kotlin, nós usamos a função `readText()` da classe `File` da biblioteca padrão do Kotlin. Aqui está um exemplo simples de como ler e imprimir o conteúdo de um arquivo de texto:

```Kotlin
import java.io.File

fun main() {
    val texto = File("meu_arquivo.txt").readText()
    println(texto)
}
```

Neste exemplo, estamos lendo o arquivo `meu_arquivo.txt` e armazenando seu conteúdo como uma string na variável `texto`. Então, usamos a função `println()` para imprimir o conteúdo na tela. Você pode executar este código em um ambiente Kotlin, como o IntelliJ IDEA ou o Kotlin Playground, para ver o resultado.

## Mergulho profundo: Entendendo o processo de leitura de arquivos de texto

Quando usamos a função `readText()` para ler um arquivo de texto, ela retorna uma string contendo o conteúdo completo do arquivo. No entanto, também é possível ler o arquivo linha por linha, usando a função `forEachLine()` da classe `BufferedReader`. Aqui está um exemplo de como fazer isso:

```Kotlin
import java.io.BufferedReader
import java.io.File

fun main() {
    val arquivo = File("meu_arquivo.txt")
    arquivo.forEachLine {
        println(it)
    }
}
```

Neste exemplo, estamos usando o método `forEachLine()` para iterar sobre as linhas do arquivo, imprimindo cada uma na tela. Isso é útil para arquivos maiores, pois evita a necessidade de armazenar todo o conteúdo como uma única string na memória.

## Veja também

- [Documentação oficial do Kotlin sobre a classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Tutorial sobre leitura de arquivos em Kotlin](https://www.baeldung.com/kotlin/read-file)

Agora que você sabe como ler arquivos de texto em Kotlin, você pode usá-lo em seus projetos para manipular dados de forma eficiente. Experimente e divirta-se programando em Kotlin!