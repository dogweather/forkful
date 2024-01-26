---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:57:07.256154-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar a existência de um diretório é uma tarefa comum para garantir que um arquivo possa ser lido ou escrito. Programadores fazem isso para evitar erros ao tentar acessar ou modificar conteúdos em um diretório inexistente.

## Como Fazer:

Java oferece métodos simples para checar a existência de diretórios. Use `Files.exists` combinado com `Paths.get`:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class CheckDirectory {
    public static void main(String[] args) {
        Path path = Paths.get("/caminho/para/o/diretorio");

        if (Files.exists(path)) {
            System.out.println("O diretório existe!");
        } else {
            System.out.println("O diretório não existe!");
        }
    }
}
```

Exemplo de saída:
```
O diretório existe!
```
ou 
```
O diretório não existe!
```

## Aprofundando:

Historicamente, antes do Java 7, muitos programadores usavam `File.exists()` do pacote `java.io`. Com o Java 7, a API `java.nio.file` foi introduzida, oferecendo o `Files.exists` junto com outras melhorias de I/O. 

Em termos de alternativas, além do `Files.exists`, você pode usar `File.isDirectory()` para verificar se um caminho é um diretório:

```java
File file = new File("/caminho/para/o/diretorio");
if (file.isDirectory()) {
    // Código aqui
}
```

Quanto aos detalhes de implementação, o método `Files.exists` trabalha com I/O de maneira robusta e menos propensa a erros. No entanto, é importante saber que o método pode ter impacto na performance se usado repetidamente em um grande número de arquivos, devido ao acesso ao disco.

## Veja Também:

- [Files.exists documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#exists-java.nio.file.Path-java.nio.file.LinkOption...-)
- [Path API in Java](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html)
- [File API in Java](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
