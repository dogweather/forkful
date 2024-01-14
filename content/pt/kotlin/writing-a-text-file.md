---
title:    "Kotlin: Escrevendo um arquivo de texto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Kotlin?

Escrever um arquivo de texto é uma tarefa comum na programação. Em Kotlin, essa tarefa pode ser feita de forma rápida e eficiente usando a biblioteca "java.io". Neste artigo, vamos explorar como escrever um arquivo de texto em Kotlin e como isso pode ser útil nas suas aplicações.

## Como fazer:

Para escrever um arquivo de texto em Kotlin, primeiro importe a biblioteca "java.io" no seu projeto. Em seguida, use a classe "FileWriter" para criar um novo arquivo de texto e adicionar o conteúdo desejado. Veja um exemplo abaixo:

```Kotlin
import java.io.FileWriter

fun main() {
    val texto = "Este é um exemplo de texto que será escrito em um arquivo."
    val arquivo = FileWriter("meu_arquivo.txt")
    arquivo.write(texto)
    arquivo.close()
}
```

Neste exemplo, importamos a biblioteca "java.io" e em seguida, criamos uma variável "texto" com uma frase de exemplo. Em seguida, criamos uma instância da classe "FileWriter" e usamos o método "write" para adicionar o conteúdo da variável "texto" ao arquivo "meu_arquivo.txt". Por fim, fechamos o arquivo usando o método "close".

Ao rodar este código, um novo arquivo de texto será criado com o conteúdo que especificamos. Caso o arquivo já exista, o conteúdo anterior será substituído. Você também pode usar o método "append" para adicionar conteúdo a um arquivo já existente.

## Mergulho profundo:

Além do exemplo mostrado acima, existem outras maneiras de escrever um arquivo de texto em Kotlin. Você pode usar as classes "BufferedWriter" ou "PrintWriter" para ter mais controle sobre a escrita no arquivo. Além disso, a biblioteca "java.nio" também fornece opções para a escrita de arquivos. É importante lembrar de sempre fechar o arquivo após a escrita, usando o método "close", para evitar possíveis erros.

Outra dica importante é usar um bloco "try-catch" ao lidar com arquivos, para tratar possíveis exceções que podem ocorrer durante a escrita ou ao fechar o arquivo.

## Veja também:

- [Documentação oficial Java para a classe FileWriter](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Tutorial sobre escrita em arquivos em Kotlin](https://www.geeksforgeeks.org/writing-text-to-file-in-kotlin/)
- [Exemplo de uso da biblioteca java.io em Kotlin](https://www.programmersought.com/article/15385321122/)