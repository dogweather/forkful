---
title:                "Lendo um arquivo de texto"
html_title:           "Java: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?
Ler um arquivo de texto em Java significa extrair informações de um arquivo de texto usando um programa escrito em Java. Os programadores fazem isso para acessar dados armazenados em um arquivo de texto, como configurações de software ou informações de um banco de dados.

## Como fazer:
```java
// Importe as classes necessárias
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
 
// Crie um BufferedReader com o arquivo desejado
BufferedReader br = new BufferedReader(new FileReader("arquivo.txt"));
 
// Leia o arquivo linha por linha
String linha;
while ((linha = br.readLine()) != null) {
  System.out.println(linha);
}
 
br.close(); // Não se esqueça de fechar o BufferedReader depois de usá-lo
```

O código acima usa um BufferedReader para ler cada linha do arquivo e imprimir seu conteúdo. É importante fechar o BufferedReader depois de usá-lo para liberar recursos.

## Aprofundando:
Ler arquivos de texto em Java é uma tarefa comum e pode ser necessária em vários projetos. Antes da versão Java 7, era necessário lidar manualmente com exceções ao ler um arquivo, mas a partir da versão 7, isso foi simplificado com a adição do bloco "try-with-resources".

Existem outras formas de ler arquivos de texto em Java, como usando as classes Scanner e FileReader, mas o BufferedReader geralmente é o mais eficiente.

É importante lembrar de manipular exceções ao ler arquivos de texto, pois erros podem ocorrer durante a leitura.

## Veja também:
- [Documentação oficial do Java sobre leitura de arquivos](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Tutorial sobre leitura de arquivos de texto em Java](https://www.baeldung.com/java-read-file)