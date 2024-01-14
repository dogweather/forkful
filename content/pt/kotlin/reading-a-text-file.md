---
title:    "Kotlin: Lendo um arquivo de texto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma tarefa comum em programação e pode ser útil por vários motivos, como obter dados de entrada de um usuário, ler arquivos de configuração ou processar dados armazenados em um arquivo.

## Como fazer?

A linguagem Kotlin possui funções nativas que facilitam a leitura de arquivos de texto. Vamos dar uma olhada em um exemplo simples de como ler um arquivo e imprimir seu conteúdo:

````Kotlin
import java.io.File

fun main() {
    val file = File("arquivo.txt")
    val linhas = file.readLines()

    for (linha in linhas) {
        println(linha)
    }
}
````

Neste exemplo, estamos importando a classe File da biblioteca Java e criando uma instância dela com o nome do arquivo que queremos ler. Em seguida, usamos o método `readLines()` para obter uma lista de todas as linhas do arquivo. Por fim, usamos um loop `for` para iterar sobre essa lista e imprimir cada linha.

A saída final será o conteúdo do arquivo de texto exibido no console. É importante lembrar que é necessário lidar com possíveis erros, como o arquivo não existir ou não ter permissão de leitura.

## Mergulho profundo

Além do método `readLines()`, a classe File também possui outras ferramentas úteis para a leitura de arquivos de texto, como o método `readText()` que retorna todo o conteúdo do arquivo como uma única string e o método `forEachLine()` que permite executar uma ação em cada linha do arquivo.

Outra opção é utilizar a biblioteca de terceiros Apache Commons IO, que possui uma gama de funções para manipulação de arquivos em Java e, por sua vez, é compatível com Kotlin.

Ler arquivos de texto pode ser um processo simples, mas é importante estar ciente de como manipular possíveis exceções e escolher a melhor abordagem de acordo com o seu objetivo.

## Veja também
- Documentação oficial da classe File em [docs.oracle.com](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Tutorial sobre leitura de arquivos em Kotlin no [Baeldung](https://www.baeldung.com/kotlin-read-file)
- Biblioteca Apache Commons IO em [commons.apache.org](https://commons.apache.org/proper/commons-io/)