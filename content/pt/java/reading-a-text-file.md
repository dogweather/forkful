---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lendo um Arquivo de Texto em Java

## O Que é e Por Quê?

Ler um arquivo de texto, no contexto de programação, significa acessar e interpretar os dados contidos no arquivo usando um programa. Isso é fundamental para programadores quando precisam trabalhar com grandes quantidades de dados armazenados em arquivos, ou quando precisam importar e exportar dados para outros sistemas.

## Como Fazer:
Aqui está um exemplo de como ler um arquivo de texto usando Java:

```Java
import java.nio.file.*;

public class LerArquivo {
    public static void main(String[] args) throws Exception {
        Path caminho = Paths.get("meuarquivo.txt");
        byte[] bytes = Files.readAllBytes(caminho);
        String conteudo = new String(bytes); 
        System.out.println(conteudo);
    }
}
```

Este programa irá ler todo o conteúdo do arquivo `meuarquivo.txt` e exibir na saída padrão.

Output:

```Java
Exemplo de texto lido do arquivo.
```

## Mergulho Profundo

Ler arquivos em Java tem sido uma necessidade desde as primeiras versões. As primeiras soluções envolviam classes como FileInputStream ou BufferedReader, mas eram complexas e propensas a erros. A implementação acima, usando `Files.readAllBytes`, foi introduzida no Java 7 como uma maneira mais simples e menos propensa a erros. É mais eficiente para arquivos pequenos ou médios por ler todo o arquivo de uma só vez, mas para arquivos muito grandes outras técnicas, como o uso de um BufferedReader, podem ser preferíveis para evitar um OutOfMemoryError.

Existem outras bibliotecas de terceiros também disponíveis para ler arquivos, como a Apache Commons IO. Ao escolher entre diferentes abordagens ou bibliotecas, os programadores devem considerar a facilidade de uso, a robustez, e o desempenho.

## Veja Também

1. Documentação oficial do Java para [Files.readAllBytes](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#readAllBytes-java.nio.file.Path-)
2. Guia completo da Oracle para [I/O e NIO](https://docs.oracle.com/javase/tutorial/essential/io/index.html)
3. Apache Commons [IO](https://commons.apache.org/proper/commons-io/) para alternativas de leitura de arquivos.
4. StackOverflow: [How to read a large text file line by line using Java?](https://stackoverflow.com/questions/5868369/how-to-read-a-large-text-file-line-by-line-using-java)