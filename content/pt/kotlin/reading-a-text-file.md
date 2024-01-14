---
title:                "Kotlin: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler um arquivo de texto é um passo fundamental na programação, pois muitas vezes precisamos acessar informações armazenadas em arquivos para processá-las ou exibi-las de alguma forma. Aprender a ler arquivos de texto é essencial para quem deseja se tornar um programador completo.

## Como fazer

Para ler um arquivo de texto em Kotlin, podemos utilizar o método readText() da classe File. Este método lê o conteúdo do arquivo e o retorna como uma única string. Veja um exemplo abaixo:

```Kotlin
val file = File("arquivo.txt")
val conteudo = file.readText()

println(conteudo)
```

No código acima, criamos um objeto File que representa o arquivo "arquivo.txt" e em seguida utilizamos o método readText() para ler seu conteúdo e o armazenamos na variável "conteudo". Depois, utilizamos o comando println() para exibir o conteúdo na tela.

## Mergulho Profundo

Além do método readText(), também existem outras formas de ler um arquivo de texto em Kotlin. Podemos utilizar o método readLines() para ler todas as linhas do arquivo e retorná-las como uma lista de strings. Veja um exemplo abaixo:

```Kotlin
val file = File("arquivo.txt")
val linhas = file.readLines()

for (linha in linhas) {
    println(linha)
}
```

Este código irá ler todas as linhas do arquivo e imprimir cada uma delas na tela. Outra opção é utilizar o método forEachLine(), que executa uma ação para cada linha do arquivo, sem a necessidade de armazená-las em uma lista.

```Kotlin
val file = File("arquivo.txt")
file.forEachLine { linha ->
    println(linha)
}
```

Além disso, podemos utilizar a classe BufferedReader para ler o arquivo linha por linha de forma mais eficiente em casos em que o arquivo é muito grande.

## Veja Também

- [Documentação oficial do Kotlin sobre leitura de arquivos](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Tutorial de leitura de arquivos em Kotlin](https://www.baeldung.com/kotlin/read-file)
- [Exemplos práticos de leitura de arquivos em Kotlin](https://github.com/SimformSolutionsPvtLtd/Kotlin-Read-and-Write-Text-To-File/)