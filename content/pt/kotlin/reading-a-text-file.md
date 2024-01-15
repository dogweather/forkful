---
title:                "Lendo um arquivo de texto"
html_title:           "Kotlin: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por que ler arquivos de texto em Kotlin?

Ler arquivos de texto é uma tarefa muito comum em programação, seja para importar dados, realizar manipulações ou simplesmente para extrair informações. Ao dominar a leitura de arquivos de texto em Kotlin, você terá uma habilidade valiosa que será útil em diversos projetos.

## Como fazer isso em Kotlin?

Em Kotlin, podemos ler arquivos de texto utilizando a classe `Scanner`, que nos permite percorrer o conteúdo do arquivo linha por linha. Para isso, primeiro precisamos criar uma instância da classe `Scanner` passando como parâmetro o caminho do arquivo que desejamos ler.

```Kotlin
val reader = Scanner(File("caminho/do/arquivo.txt"))
```

Em seguida, podemos utilizar um laço de repetição para percorrer as linhas do arquivo através do método `nextLine()`.

```Kotlin
while (reader.hasNextLine()) {
    val linha = reader.nextLine()
    //fazer manipulações com a linha lida
}
```

Podemos também utilizar o método `useLines()` para ler o arquivo diretamente em formato de lista, o que facilita o acesso aos dados.

```Kotlin
val linhas = File("caminho/do/arquivo.txt").useLines { it.toList() }
```

## Profundidade na leitura de arquivos em Kotlin

Além da leitura básica de arquivos de texto, é possível realizar diversas operações mais complexas utilizando as ferramentas disponíveis em Kotlin. Por exemplo, podemos ler somente uma parte específica do arquivo, pular linhas e até mesmo utilizar expressões regulares para encontrar padrões dentro do conteúdo.

Também é importante lembrar de sempre fechar o arquivo após a sua leitura, utilizando o método `close()`, ou utilizar a estrutura de controle `try-with-resources` para garantir que o arquivo será sempre fechado, mesmo em caso de erro durante a leitura.

# Veja também

- [Documentação oficial do Kotlin sobre leitura de arquivos](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Tutorial sobre leitura de arquivos em Kotlin](https://www.baeldung.com/kotlin-read-file)
- [Exemplos práticos de leitura de arquivos em Kotlin](https://dev.to/caitlynmayers/reading-files-in-kotlin-55ia)