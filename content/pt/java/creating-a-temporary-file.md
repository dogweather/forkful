---
title:                "Java: Criando um arquivo temporário"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Java?

Criar um arquivo temporário é útil em casos onde precise armazenar dados temporariamente durante a execução de um programa em Java. Isso pode ser útil, por exemplo, para salvar informações temporárias durante a criação de um arquivo permanente ou para armazenar informações que serão usadas apenas temporariamente.

## Como criar um arquivo temporário em Java

Para criar um arquivo temporário em Java, podemos usar a classe `File` e o método `createTempFile()`. Este método cria um arquivo vazio em um local temporário do sistema e retorna um objeto `File` que representa esse arquivo.

```Java
import java.io.File;
import java.io.IOException;

public class CriarArquivoTemporario {

    public static void main(String[] args) throws IOException {

        //criando um arquivo temporário
        File arquivoTemporario = File.createTempFile("arquivo", ".txt");

        //definindo o caminho do arquivo
        System.out.println("Caminho do arquivo temporário: " + arquivoTemporario.getAbsolutePath());

    }
}
```

A saída deste código será algo parecido com: `Caminho do arquivo temporário: C:\Users\NomeUsuario\AppData\Local\Temp\arquivo8654123310510021621.txt`. Vale ressaltar que o nome e a extensão do arquivo temporário serão gerados aleatoriamente.

## Explorando mais a criação de um arquivo temporário em Java

Além do método `createTempFile()`, a classe `File` também possui outros métodos úteis para trabalhar com arquivos temporários, como `deleteOnExit()`, que exclui o arquivo temporário quando o programa é encerrado, e `length()`, que retorna o tamanho do arquivo em bytes.

```Java
import java.io.File;
import java.io.IOException;

public class CriarArquivoTemporario {

    public static void main(String[] args) throws IOException {

        //criando um arquivo temporário
        File arquivoTemporario = File.createTempFile("arquivo", ".txt");

        //excluindo o arquivo temporário quando o programa é encerrado
        arquivoTemporario.deleteOnExit();

        //imprimindo o tamanho do arquivo
        System.out.println("Tamanho do arquivo temporário: " + arquivoTemporario.length() + " bytes");

    }
}
```

É importante lembrar que os arquivos temporários criados pelo método `createTempFile()` serão excluídos automaticamente pelo sistema quando o programa terminar, porém, se for desejado excluir o arquivo em outro momento, é possível utilizar o método `delete()` da classe `File`.

## Veja também

- [Documentação oficial do Java para a classe File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Tutorial sobre arquivos temporários em Java](https://www.devmedia.com.br/arquivo-temporario-em-java/31322)
- [Vídeo tutorial sobre como criar arquivos temporários em Java](https://www.youtube.com/watch?v=RfN8hGe6-B0)