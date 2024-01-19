---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Criar um arquivo temporário é a prática de formar um arquivo que existirá apenas para a duração da sessão atual do programa. Programadores fazem isso quando precisam armazenar dados temporários que não precisam ser permanentes.

## Como Fazer:

Vamos ver como podemos criar um arquivo temporário em Java. Para isso, vamos usar a classe `File` da biblioteca Java IO.

```Java
import java.io.File;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        try {
            // Criando um arquivo temporário
            File tempFile = File.createTempFile("tempFile", ".txt");

            // Imprime o caminho do arquivo
            System.out.println("Caminho do arquivo temporário: " + tempFile.getAbsolutePath());

            // Verificando se o arquivo é temporário
            System.out.println("É temporário? " + tempFile.deleteOnExit());
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Ao executar o código acima, você verá uma saída semelhante à seguinte:

```Java
Caminho do arquivo temporário: /tmp/tempFile1234567890.txt
É temporário? true
```

## Mergulho Profundo

No contexto histórico, a criação de arquivos temporários surgiu da necessidade de armazenar dados temporários que poderiam ser excluídos após o uso. Isso é especialmente útil quando não se quer sobrecarregar a memória principal.

Existem várias maneiras de criar arquivos temporários. Além do método `createTempFile` da classe `File`, também podemos usar a biblioteca `Files` do Java NIO. 

No que diz respeito aos detalhes de implementação, o método `createTempFile` cria um arquivo vazio, o nome do arquivo será gerado automaticamente para garantir que o arquivo seja único e evitar conflitos de nomenclatura.

## Veja também

Para obter mais informações e exemplos relacionados à criação de arquivos temporários em Java, consulte os seguintes links:

1. Documentação oficial da Oracle para a classe File: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
2. Guia de arquivos temporários da Oracle: https://docs.oracle.com/javase/tutorial/essential/io/file.html#creating
3. Documentação oficial da Oracle para a classe Files: https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html