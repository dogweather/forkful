---
title:                "Trabalhando com csv"
html_title:           "Java: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que

CSV (Comma-Separated Values) é um formato de arquivo utilizado para armazenar e transferir dados tabulares de forma simples e compatível com uma ampla gama de softwares. Trabalhar com CSV no Java pode ser útil para importar e exportar dados de um banco de dados, gerar relatórios ou realizar integrações com outras aplicações. 

## Como fazer

Para trabalhar com CSV no Java, existem diversas bibliotecas disponíveis. Uma das mais populares é a Apache Commons CSV, que oferece uma API simples e fácil de usar. O primeiro passo é adicionar a dependência da biblioteca em seu projeto. No Maven, basta adicionar a seguinte linha no arquivo pom.xml:
```Java
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.8</version>
</dependency>
```

Com a biblioteca adicionada, podemos começar a ler e escrever em arquivos CSV. Veja um exemplo de como ler um arquivo CSV com a biblioteca Apache Commons CSV e imprimir seus dados:
```Java
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

public static void main(String[] args) throws IOException {
    // Criando um parser para o arquivo utilizando o formato padrão de CSV
    CSVParser parser = CSVParser.parse(new FileReader("caminho/do/arquivo.csv"), CSVFormat.DEFAULT);
    // Iterando sobre as linhas do arquivo
    for (CSVRecord record : parser) {
        // Imprimindo os dados da linha atual
        System.out.print(record.get(0)); // imprime a primeira coluna
        System.out.print(record.get("coluna")); // imprime a coluna com o nome "coluna"
    }
}
```

Além de ler, também podemos escrever em arquivos CSV com a biblioteca Apache Commons CSV. Veja um exemplo de como criar um arquivo CSV e adicionar dados a ele:
```Java
import java.io.FileWriter;
import java.io.IOException;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

public static void main(String[] args) throws IOException {
    // Criando um writer para o arquivo utilizando o formato padrão de CSV
    CSVPrinter printer = new CSVPrinter(new FileWriter("caminho/do/arquivo.csv"), CSVFormat.DEFAULT);
    // Adicionando dados ao arquivo
    printer.printRecord("dado1", "dado2", "dado3"); // adiciona uma linha com os dados na ordem especificada
    printer.printRecord("dado4", "dado5", "dado6");
    // Fechando o writer
    printer.close();
}
```

## Aprofundando-se

Trabalhar com CSV pode ser ainda mais interessante quando precisamos lidar com situações mais complexas, como arquivos com diferentes tipos de dados, delimitadores especiais ou até mesmo dados faltantes. A biblioteca Apache Commons CSV oferece diversos métodos e configurações para lidar com esses casos, por isso é importante ter conhecimento sobre suas funcionalidades. 

Além disso, é possível utilizar outras bibliotecas no Java para trabalhar com CSV, como por exemplo a opencsv e a Super CSV. Vale a pena explorar essas opções para escolher qual melhor se adequa às suas necessidades.

## Consulte também

- [Documentação da biblioteca Apache Commons CSV](https://commons.apache.org/proper/commons-csv)
- [Exemplos de código com CSV no Java](https://www.baeldung.com/apache-commons-csv)
- [Comparação entre diferentes bibliotecas de CSV em Java](https://www.baeldung.com/java-csv-libraries)