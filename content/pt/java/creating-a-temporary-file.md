---
title:    "Java: Criando um arquivo temporário"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Java?

Criar um arquivo temporário pode ser útil em várias situações, como armazenar dados temporários que serão usados em outros programas ou como uma medida de segurança para evitar perda de dados.

## Como criar um arquivo temporário em Java

Para criar um arquivo temporário em Java, podemos utilizar a classe `File` e o método `createTempFile()`. Veja o seguinte exemplo de código:

```Java
import java.io.File;
import java.io.IOException;

public class ArquivoTemporario {
    public static void main(String[] args) {
        try {
            // Cria um arquivo temporário com o prefixo "temp" e a extensão ".txt"
            File tempFile = File.createTempFile("temp", ".txt");
            // Define que o arquivo será apagado na finalização do programa
            tempFile.deleteOnExit();
            // Escreve uma mensagem no arquivo
            System.out.println("Arquivo temporário criado em: " + tempFile.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Ao executar o código acima, será impresso o caminho do arquivo temporário criado, que geralmente está na pasta do sistema operacional em uso. O método `deleteOnExit()` é utilizado para garantir que o arquivo será apagado ao finalizar o programa, evitando acúmulo de arquivos temporários no sistema.

## Aprofundando na criação de arquivos temporários em Java

Além do método `createTempFile()`, existem outras maneiras de criar arquivos temporários em Java, como utilizando a classe `Files` ou a interface `TemporaryFilesystem` do pacote `org.apache.commons.io`. Também é possível especificar o diretório onde o arquivo temporário será criado ou definir seus atributos, como permissões de leitura e escrita.

É importante lembrar que arquivos temporários podem conter informações sensíveis e, portanto, devem ser tratados com cautela. Além disso, é recomendado sempre apagar o arquivo temporário após o seu uso para evitar problemas de segurança ou espaço em disco.

## Veja também

- [Documentação oficial do método `createTempFile()`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.io.File))
- [Tutorial sobre a criação de arquivos temporários em Java](https://www.baeldung.com/java-temporary-file)
- [Exemplos de utilização da classe `Files`](https://www.tutorialspoint.com/java/io/java_io_files.htm)
- [Documentação da interface `TemporaryFilesystem`](https://commons.apache.org/proper/commons-io/apidocs/org/apache/commons/io/file/TemporaryFilesystem.html)