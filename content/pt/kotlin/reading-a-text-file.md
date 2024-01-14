---
title:    "Kotlin: Lendo um arquivo de texto"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que ler arquivos de texto?

Ler arquivos de texto é uma habilidade essencial para qualquer programador Kotlin. Isso permite que você armazene e manipule dados em um formato fácil de entender e utilizar. Além disso, a leitura de arquivos de texto pode ser útil em diversas situações, como em casos em que a interação com um usuário não é possível ou em que os dados a serem manipulados são muito grandes para serem inseridos manualmente. 

## Como ler um arquivo de texto em Kotlin

Para ler um arquivo de texto em Kotlin, primeiro precisamos criar um objeto do tipo `File` e passar o caminho do arquivo como parâmetro. Em seguida, podemos usar o método `useLines()` para ler as linhas do arquivo e realizar as operações necessárias. Veja um exemplo abaixo:

```Kotlin
val file = File("caminho/do/arquivo.txt")

file.useLines { lines ->
    lines.forEach { line ->
        println(line)
    }
}
```

O código acima irá imprimir todas as linhas do arquivo na saída do console. Você pode adaptar esse código para executar outras operações de acordo com a sua necessidade.

## Aprofundando na leitura de arquivos de texto

Além do método `useLines()`, Kotlin também possui outras maneiras de ler arquivos de texto, como por exemplo o método `readText()`, que retorna todo o conteúdo do arquivo como uma `String`. Também é possível usar a classe `BufferedReader`, que oferece mais opções de controle durante a leitura do arquivo.

É importante lembrar que ao ler um arquivo de texto, devemos sempre lidar com possíveis exceções, como por exemplo o `FileNotFoundException`. Portanto, é recomendável que você utilize o bloco `try-catch` para tratar essas exceções de forma adequada. 

## Veja também

- [Documentação oficial do Kotlin para leitura de arquivos](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Tutorial do Kotlin sobre leitura de arquivos de texto](https://kotlinlang.org/docs/tutorials/kotlin-for-py/read-write-files.html)
- [Exemplos práticos de leitura de arquivos em Kotlin](https://www.javatpoint.com/kotlin-file-io)