---
title:                "Kotlin: Escrevendo um arquivo de texto"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto utilizando Kotlin?

Escrever um arquivo de texto pode ser útil em diversas situações, seja para armazenar dados, gerar relatórios ou criar documentos. Utilizando a linguagem de programação Kotlin, é possível realizar essa tarefa de forma simples e eficiente. Neste post, vamos mostrar como escrever um arquivo de texto utilizando Kotlin e também entender um pouco mais sobre o processo.

## Como escrever um arquivo de texto utilizando Kotlin

Para escrever um arquivo de texto em Kotlin, é necessário utilizar a classe `File` do pacote `java.io`. Primeiramente, é preciso criar uma instância da classe e especificar o caminho e o nome do arquivo que será criado. Por exemplo:

```Kotlin
val arquivo = File("/caminho/para/arquivo.txt")
```

Em seguida, é necessário utilizar o método `writeText()` para escrever o conteúdo desejado no arquivo. Por exemplo:

```Kotlin
arquivo.writeText("Este é o conteúdo do meu arquivo de texto.")
```

Também é possível utilizar o método `appendText()` para adicionar conteúdo ao final do arquivo, sem apagar o que já estava escrito. Por exemplo:

```Kotlin
arquivo.appendText("Este é o conteúdo adicionado ao final do arquivo.")
```

Além disso, é importante lembrar de sempre fechar o arquivo após a escrita, utilizando o método `close()`, para garantir que tudo foi salvo corretamente.

Por fim, para verificar se o arquivo foi criado e o conteúdo foi escrito corretamente, você pode utilizar o método `readText()`. Ele retornará uma `String` com o conteúdo do arquivo. Por exemplo:

```Kotlin
val conteudoArquivo = arquivo.readText()
println(conteudoArquivo)
```

O código completo ficaria assim:

```Kotlin
val arquivo = File("/caminho/para/arquivo.txt")
arquivo.writeText("Este é o conteúdo do meu arquivo de texto.")
arquivo.appendText("Este é o conteúdo adicionado ao final do arquivo.")
arquivo.close()

val conteudoArquivo = arquivo.readText()
println(conteudoArquivo)
```

Se tudo der certo, você deve ver um output parecido com o seguinte:

```
Este é o conteúdo do meu arquivo de texto.
Este é o conteúdo adicionado ao final do arquivo.
```

## Deep Dive

Agora que já vimos como escrever um arquivo de texto em Kotlin, vamos entender um pouco mais sobre o processo. Em primeiro lugar, é importante notar que o código utilizado anteriormente será sincrono, ou seja, o programa ficará bloqueado até que o arquivo seja escrito e fechado corretamente. Se você deseja escrever um arquivo de forma assíncrona, existem algumas opções, como utilizar a classe `AsyncTextFileWriter`, do pacote `kotlinx.coroutines.io`, ou utilizar a função `writeText()` em um escopo de coroutine.

Outro ponto importante é a questão do encoding do arquivo de texto. Por padrão, o Kotlin utiliza o encoding UTF-8 para escrever e ler arquivos de texto, mas é possível especificar outro encoding utilizando uma sobrecarga dos métodos `writeText()` e `readText()`.

Por fim, vale lembrar que, ao escrever em um arquivo de texto já existente, todo o seu conteúdo anterior será substituído, a menos que seja utilizado o método `appendText()`, como mostrado anteriormente.

## Veja também

- [Documentação oficial do Kotlin sobre a classe `File`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Documentação oficial do Kotlin sobre a classe `AsyncTextFileWriter`](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-io/kotlinx.coroutines.io/-async-text-file-writer/)
- [Pacote `java.io` na documentação oficial do Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/package-summary.html)