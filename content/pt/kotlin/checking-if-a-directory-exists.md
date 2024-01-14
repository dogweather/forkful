---
title:                "Kotlin: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a situação de precisar verificar se um diretório existe em seu código Kotlin? Isso pode ser útil para garantir que o diretório necessário para a execução do programa esteja presente ou para evitar erros ao tentar acessar um diretório inexistente. Neste post, vamos explicar como fazer isso de forma simples e eficiente com alguns exemplos de código.

## Como Fazer

Para verificar se um diretório existe em Kotlin, podemos utilizar a classe `File` e seu método `isDirectory()`. Veja um exemplo de código abaixo:

```Kotlin
val directory = File("caminho/do/diretorio")
if (directory.isDirectory()) {
    println("O diretório existe!")
} else {
	println("O diretório não existe.")
}
```

Caso o diretório exista, o código acima irá imprimir "O diretório existe!". Caso contrário, será impresso "O diretório não existe.". É importante ressaltar que o método `isDirectory()` também só retorna `true` se o diretório existir, pois se ele não existir, a classe `File` irá criar um novo arquivo com o nome informado.

Outra opção é utilizar o método `exists()` da classe `File`, que irá retornar `true` se o arquivo ou diretório existir ou `false` caso contrário. Veja um exemplo:

```Kotlin
val directory = File("caminho/do/diretorio")
if (directory.exists()) {
    if (directory.isDirectory()) {
        println("O diretório existe!")
    } else {
        println("O caminho informado é um arquivo, não um diretório.")
    }
} else {
    println("O diretório não existe.")
}
```

Como podemos observar, além de verificar se o diretório existe, esse código também nos dá a opção de checar se o caminho informado corresponde a um arquivo ou a um diretório.

## Deep Dive

Agora que já sabemos como verificar se um diretório existe em Kotlin, é importante entendermos como os métodos `isDirectory()` e `exists()` funcionam. Ambos utilizam os métodos nativos do sistema operacional para realizar a verificação, ou seja, eles se baseiam no sistema de arquivos do dispositivo onde o programa está sendo executado.

Dessa forma, se o programa estiver sendo executado em diferentes dispositivos, é possível que os resultados da verificação sejam diferentes. Por exemplo, em um dispositivo móvel, pode ser que o diretório exista, mas em um computador ele não exista. Por isso, é importante levar em consideração o contexto e o sistema de arquivos do dispositivo ao realizar essas verificações.

## Veja Também

- [Documentação oficial do método `isDirectory()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/is-directory.html)
- [Documentação oficial do método `exists()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- [Exemplo de uso da classe `File`](https://www.journaldev.com/10776/kotlin-file-io-operations-examples)