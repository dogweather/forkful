---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:57:29.903585-07:00
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar se um diretório existe é o processo de checar se um caminho específico no sistema de arquivos aponta para uma pasta real. Programadores fazem isso para evitar erros de arquivo não encontrado ou para decidir se criam um novo diretório.

## Como Fazer:
```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/caminho/para/o/diretorio")
    
    if (Files.exists(path)) {
        println("Diretório existe!")
    } else {
        println("Diretório não existe.")
    }
}
```
Output caso exista:
```
Diretório existe!
```
Output caso não exista:
```
Diretório não existe.
```

## Mergulho Profundo
No início, checar a existência de um diretório era uma operação mais dependente do sistema operacional. Com o Java NIO (New Input/Output), introduzido no Java 7 e também disponível em Kotlin, abstraímos essas verificações para serem mais portáveis entre diferentes SOs. Alternativas? Use `File.exists()` para abordagens mais antigas ou antes do Java NIO. Quanto aos detalhes de implementação, o `Files.exists()` checa as permissões e pode lançar `SecurityException` se o acesso ao diretório for negado, algo a ter em mente.

## Veja Também
- [Java Platform SE 8 - Files.exists](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#exists-java.nio.file.Path-java.nio.file.LinkOption...-)
- [Kotlin API - File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
