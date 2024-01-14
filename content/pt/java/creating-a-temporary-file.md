---
title:                "Java: Criando um arquivo temporário."
simple_title:         "Criando um arquivo temporário."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Java?

Os arquivos temporários são úteis para armazenar informações temporárias durante a execução de um programa Java. Eles podem ser usados para armazenar dados que serão utilizados em uma determinada parte do código e depois descartados, evitando assim a sobrecarga de memória.

## Como criar um arquivo temporário em Java
Criar um arquivo temporário em Java é um processo simples que pode ser realizado seguindo esses três passos:

1. Importe a classe "java.io.File":

```Java
import java.io.File;
```

2. Crie um objeto do tipo File e especifique o local onde deseja criar o arquivo temporário:

```Java
File tempFile = new File("caminho/do/arquivo/temporario");
```

3. Use o método ".createTempFile()" para criar o arquivo temporário:

```Java
tempFile.createTempFile("prefixo", "extensao");
```

Após executar esses três passos, o arquivo temporário será criado e poderá ser utilizado no restante do seu código.

## Mergulho Profundo
Para uma visão mais aprofundada sobre a criação de arquivos temporários em Java, aqui estão algumas informações adicionais a serem consideradas:

- O método ".createTempFile()" também permite especificar um diretório para armazenar o arquivo temporário ao invés de criar um no diretório atual.

- Você também pode especificar um prefixo e uma extensão personalizadas para o nome do arquivo temporário.

- É importante lembrar de remover o arquivo temporário após utilizá-lo por completo para evitar acumulação de arquivos indesejados.

## Veja também
- [Documentação Java: File class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Artigo: How to create a temporary file in Java](https://www.baeldung.com/java-temporary-file)
- [Discussão no Stack Overflow sobre criação de arquivos temporários em Java](https://stackoverflow.com/questions/617414/create-a-temporary-directory-in-java)