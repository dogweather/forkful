---
title:                "Java: Verificando se um diretório existe"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Quando se está escrevendo um programa em Java, pode ser necessário verificar se um determinado diretório existe antes de executar uma ação específica. Por exemplo, antes de criar um novo arquivo em um diretório, é importante garantir que o diretório realmente exista para evitar erros e falhas no programa.

## Como fazer isso:

Para verificar se um diretório existe em Java, existem algumas opções. Uma delas é utilizar o método `exists()` da classe `java.io.File`. Este método retorna um booleano indicando se o diretório especificado realmente existe ou não. Veja um exemplo abaixo:

```Java
import java.io.File;

public class VerificarDiretorio {

    public static void main(String[] args) {

        // Caminho do diretório que será verificado
        String caminho = "C:\\meudiretorio";

        // Cria uma instância da classe File com o caminho especificado
        File file = new File(caminho);

        // Verifica se o diretório existe e imprime o resultado
        if (file.exists()) {
            System.out.println("O diretório " + caminho + " existe!");
        } else {
            System.out.println("O diretório " + caminho + " não existe!");
        }
    }
}
```
**Saída:**
```
O diretório C:\meudiretorio existe!
```

Além do método `exists()`, também é possível utilizar outras classes e métodos para verificar a existência de um diretório, como por exemplo utilizando a classe `java.nio.file.Files` e o método `exists()`, ou utilizando a classe `java.nio.file.Path` e o método `toFile().exists()`. A escolha do melhor método depende do seu contexto e das necessidades do seu programa.

## Uma exploração mais profunda:

Uma questão importante a ser considerada ao verificar a existência de um diretório é a diferença entre diretórios físicos e lógicos. Enquanto diretórios físicos são aqueles que existem realmente no sistema operacional, diretórios lógicos podem ser definidos pelo programa e não existir fisicamente no sistema.

Além disso, ao verificar a existência de um diretório no Java, é importante levar em conta as permissões de acesso do usuário. Se o usuário não tiver permissão de leitura em um determinado diretório, o método `exists()` irá retornar `false`, mesmo que o diretório exista fisicamente no sistema.

Portanto, é importante estar ciente dessas nuances ao verificar a existência de um diretório em Java.

## Veja também:

- Documentação oficial do método `exists()` da classe `java.io.File`: https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--
- Tutorial sobre a classe `java.nio.file.Files` e o método `exists()`: https://www.baeldung.com/java-directory-exists
- Documentação oficial da classe `java.nio.file.Path`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html