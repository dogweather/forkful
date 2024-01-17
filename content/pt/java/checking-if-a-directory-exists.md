---
title:                "Verificando se um diretório existe"
html_title:           "Java: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Verificar se um diretório existe é uma tarefa comum em programação. Ela envolve verificar se um determinado caminho ou endereço se refere a um diretório existente no sistema de arquivos. Os programadores geralmente fazem isso para garantir que suas aplicações funcionem corretamente e possam manipular os arquivos e pastas necessários.

## Como fazer:
```
// Importar a classe necessária
import java.io.File;

// Definir o caminho do diretório a ser verificado
String path = "/caminho/do/diretorio";

// Criar um objeto File utilizando o caminho
File directory = new File(path);

// Verificar se o diretório existe
if (directory.exists()) {
    System.out.println("O diretório existe.");
} else {
    System.out.println("O diretório não existe.");
}
```

## Mergulho Profundo:
Verificar se um diretório existe é uma tarefa importante em programação, pois isso permite que o programa saiba como lidar com os arquivos e pastas no sistema de arquivos. Antes do Java 7, a maneira mais comum de verificar se um diretório existia era por meio do método `exists()` da classe `File` como mostrado acima. No entanto, a partir do Java 7, o método `exists()` foi depreciado e recomenda-se o uso do método `isDirectory()` da classe `Files` para verificar se um diretório existe. Outra alternativa é utilizar a classe `Path` em conjunto com o método `Files.exists()`.

## Veja também:
- [Documentação Oracle sobre a classe File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Documentação Oracle sobre a classe Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Tutorial sobre como verificar se um diretório existe em Java](https://www.mkyong.com/java/how-to-check-if-directory-exists-in-java/)