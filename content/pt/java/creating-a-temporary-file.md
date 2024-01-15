---
title:                "Criando um arquivo temporário"
html_title:           "Java: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Java?

Existem várias situações em que é necessário criar um arquivo temporário em um programa Java. Isso pode ser útil para armazenar dados temporários, criar um backup antes de fazer alterações permanentes ou até mesmo para lidar com arquivos grandes sem comprometer a memória do sistema.

## Como criar um arquivo temporário em Java

Você pode criar um arquivo temporário em Java usando a classe `File` e o método `createTempFile()`. Veja um exemplo de código abaixo:

```Java 
File tempFile = File.createTempFile("temp", ".txt");
```

O primeiro parâmetro do método `createTempFile()` é um prefixo para o nome do arquivo temporário, enquanto o segundo parâmetro é a extensão do arquivo. O arquivo será criado no diretório padrão para arquivos temporários do sistema operacional.

## Aprofundando no assunto

Além do método `createTempFile()`, existem outras formas de criar arquivos temporários em Java. Por exemplo, a classe `Files` possui um método `createTempFile()` que permite definir o diretório onde o arquivo será criado. Também é possível usar a classe `RandomAccessFile` para criar um arquivo temporário e escrever dados nele.

Ao criar um arquivo temporário, é importante lembrar de excluí-lo depois de utilizá-lo. Para isso, você pode usar o método `deleteOnExit()` da classe `File` ou o método `delete()` após realizar todas as operações necessárias.

## Veja também

- Documentação oficial da classe `File`: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Tutorial sobre como criar e manipular arquivos em Java: https://www.baeldung.com/java-io-write-file