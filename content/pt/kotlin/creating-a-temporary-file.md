---
title:    "Kotlin: Creating a temporary fileCriando um arquivo temporário"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

##Por que criar um arquivo temporário em Kotlin?

Criar um arquivo temporário é útil quando se precisa armazenar dados temporários durante a execução de um programa. Esse tipo de arquivo é apagado automaticamente após o uso, evitando a acumulação de arquivos desnecessários no sistema.

##Como criar um arquivo temporário em Kotlin

Para criar um arquivo temporário em Kotlin, podemos utilizar a classe `File` e o método `createTempFile()`. Veja o exemplo abaixo:

```Kotlin
val arquivoTemporario = File.createTempFile("temp", ".txt")
println("Arquivo criado: ${arquivoTemporario.absolutePath}")
```

A saída do código acima será algo como:

```
Arquivo criado: /var/folders/t4/q1zz4ndj6fn3z8fvq8d99mdn6w54qb/T/temp1439144926254520346.txt
```

Podemos também especificar o diretório em que o arquivo temporário será criado, como no exemplo abaixo:

```Kotlin
val diretorio = "/Users/usuario/Documentos"
val arquivoTemporario = File.createTempFile("temp", ".txt", File(diretorio))
println("Arquivo criado: ${arquivoTemporario.absolutePath}")
```

Com isso, o arquivo temporário será criado no diretório específico que escolhemos.

##Aprofundando um pouco mais

Além do método `createTempFile()`, a classe `File` também possui o método `createTempDirectory()`, que permite a criação de um diretório temporário da mesma forma que um arquivo temporário.

Ao criar um arquivo temporário, também podemos especificar um prefixo e um sufixo para o nome do arquivo, ao invés de apenas o nome e a extensão. Isso pode ser feito utilizando o método `createTempFile()` da seguinte forma:

```Kotlin
val arquivoTemporario = File.createTempFile("temp_", "_relatorio", File(diretorio))
```

O nome do arquivo temporário gerado será algo como "temp_1439144926254520346_relatório.txt", tornando mais fácil identificar qual arquivo é o temporário.

Podemos ver que a criação de arquivos temporários em Kotlin é bastante simples e nos oferece diversas opções para personalizar o processo.

##Veja também
- [Documentação oficial do método `createTempFile()` da classe `File`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Tutorial sobre a criação de arquivos temporários em Kotlin](https://www.devmedia.com.br/criando-arquivos-temporarios-em-java/25153)
- [Guia completo sobre manipulação de arquivos em Kotlin](https://www.baeldung.com/kotlin-file-io)