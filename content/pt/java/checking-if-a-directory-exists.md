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

## O Que & Por Quê?

Verificar se um diretório existe é uma operação de consulta do sistema de arquivos para confirmar a existência do diretório especificado. Os programadores fazem isso para evitar erros e exceções ao tentar acessar ou manipular diretórios que não existem.

## Como Fazer:

Aqui está um exemplo de como você pode verificar se um diretório existe em Java.

```Java
import java.nio.file.Files;
import java.nio.file.Paths;

public class CheckDirectory {
    public static void main(String[] args) {
        if(Files.exists(Paths.get("/caminho/do/diretorio"))) {
            System.out.println("O diretório existe.");
        } else {
            System.out.println("O diretório não existe.");
        }
    }
}
```
Na saída, você verá "O diretório existe." se o diretório existir, ou "O diretório não existe." se não existir.

## Aprofundamento:

Verificar a existência de um diretório é uma prática fundamental em programação desde os primórdios dos sistemas operacionais baseados em diretórios.

Uma alternativa ao código que vimos seria usar a classe `File` em vez de `Paths` e `Files`:

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        File dir = new File("/caminho/do/diretorio");
        if(dir.exists()) {
            System.out.println("O diretório existe.");
        } else {
            System.out.println("O diretório não existe.");
        }
    }
}
```

No entanto, o uso das classes `Paths` e `Files` é mais moderno e geralmente preferido. O histórico por trás disso pode ser encontrado [aqui](https://docs.oracle.com/javase/tutorial/essential/io/legacy.html). Não se esqueça, sempre verifique se um diretório existe antes de executar operações nele.

## Veja Também:

* [Documentação Oracle Paths](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Paths.html)
* [Documentação Oracle Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
* [Documentação Oracle File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)