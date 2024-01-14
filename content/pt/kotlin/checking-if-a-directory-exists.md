---
title:    "Kotlin: Verificando se um diretório existe"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Ao escrever um programa em Kotlin, muitas vezes pode ser necessário verificar se um diretório existe antes de realizar operações nele. Isso pode ser especialmente útil quando lidando com arquivos e pastas criados dinamicamente, ou quando se trabalha com uma estrutura de diretório complexa. Neste artigo, discutiremos como verificar a existência de um diretório em Kotlin, para que você possa implementar essa funcionalidade em seus próprios projetos.

## Como fazer

A verificação da existência de um diretório pode ser feita de forma simples em Kotlin, utilizando a classe `File`. Para isso, primeiro importe a classe `File` em seu código:

```Kotlin
import java.io.File
```

Em seguida, declare um objeto `File` que represente o diretório que você deseja verificar:

```Kotlin
val directory = File("caminho/para/o/seu/diretorio")
```

Agora, podemos utilizar o método `exists()` da classe `File` para verificar se o diretório existe:

```Kotlin
if(directory.exists()) {
  println("O diretório existe!")
} else {
  println("O diretório não existe.")
}
```

Se o diretório existir, o programa imprimirá "O diretório existe!". Caso contrário, será impressa a mensagem "O diretório não existe.".

## Aprofundando

Ao utilizar o método `exists()`, é importante estar ciente de que ele fará a verificação em tempo real. Isso significa que, se o diretório for criado ou excluído durante a execução do programa, o resultado da verificação também será alterado.

Outro ponto importante é que o método `exists()` também pode retornar `true` para outros tipos de arquivos, como arquivos regulares ou links simbólicos. Portanto, é importante estar ciente do tipo de objeto que está sendo verificado.

Caso você precise verificar apenas a existência de diretórios, pode utilizar o método `isDirectory()`:

```Kotlin
if(directory.isDirectory()) {
  println("É um diretório!")
} else {
  println("Não é um diretório.")
}
```

## Veja também

- Documentação oficial da classe `File`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html
- Tutorial de Kotlin: https://kotlinlang.org/docs/tutorials/getting-started.html
- Mais tutoriais de Kotlin: https://www.tutorialspoint.com/kotlin/index.htm