---
title:    "Kotlin: Gerando um arquivo temporário"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Kotlin?

Há momentos em que precisamos armazenar dados temporários em nossos programas em Kotlin. Isso pode ser necessário por vários motivos, como:
- Armazenamento de dados em buffer
- Gerenciamento de arquivos grandes
- Realização de testes

Felizmente, Kotlin oferece uma maneira fácil de criar arquivos temporários. Neste post, vamos explorar como fazer isso.

## Como criar um arquivo temporário em Kotlin

Para criar um arquivo temporário em Kotlin, podemos usar a classe `File` do pacote `java.io`. O código a seguir mostra como criar um arquivo temporário e escrever dados nele:

```Kotlin
import java.io.File
import java.io.FileWriter

fun main() {
    val tempFile = File.createTempFile("meu_arquivo_temp", ".txt")
    val writer = FileWriter(tempFile)
    writer.write("Estou criando um arquivo temporário em Kotlin!")
    writer.close()
}
```

Ao executar este código, um arquivo temporário com o nome "meu_arquivo_temp" e a extensão ".txt" será criado na pasta padrão do sistema operacional. Podemos especificar um diretório diferente passando o caminho para ele como segundo argumento para o método `createTempFile()`.

Além disso, podemos usar o arquivo temporário criado para armazenar dados em buffer ou realizar outras operações de arquivo necessárias em nosso programa.

## Aprofundando-se na criação de arquivos temporários em Kotlin

Agora que sabemos como criar um arquivo temporário em Kotlin, vamos dar uma olhada em alguns detalhes importantes.

### Prefixo e sufixo

Ao criar um arquivo temporário em Kotlin, podemos fornecer um prefixo e um sufixo para o nome do arquivo. Por padrão, o prefixo é "tmp" e o sufixo é ".tmp". No entanto, podemos fornecer nossos próprios valores, conforme necessário.

### Deletando o arquivo temporário

Quando um arquivo temporário é criado, ele não é excluído automaticamente após o término do programa. É responsabilidade do programador excluir o arquivo quando ele não for mais necessário. Podemos fazer isso chamando o método `delete()` na instancia do objeto `File`.

### Gerando nomes aleatórios

Também podemos gerar automaticamente nomes aleatórios para nossos arquivos temporários, passando `null` no lugar do prefixo e sufixo ao chamar o `createTempFile()`. Isso garante que sempre teremos um nome único para o nosso arquivo temporário.

## Veja também

- [Documentação oficial sobre criação de arquivos temporários em Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Tutorial sobre manipulação de arquivos em Kotlin](https://www.baeldung.com/kotlin-file-io)

Esperamos que este post tenha sido útil para entender como criar e usar arquivos temporários em Kotlin. Agora você pode usar essa técnica em seus programas para armazenar dados temporários de forma eficiente.