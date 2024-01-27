---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com CSV significa lidar com "Comma-Separated Values", um formato prático para armazenar tabelas de dados. Programadores usam isso pela simplicidade e ampla adoção em ferramentas de planilhas, bancos de dados e importação/exportação de dados.

## Como Fazer:
Primeiro, para ler um arquivo CSV:

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class CSVReader {
    public static void main(String[] args) {
        String path = "seu-arquivo.csv";
        String line = "";

        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                System.out.println(Arrays.toString(values));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Saída:
```
[valor1, valor2, valor3]
[valor4, valor5, valor6]
...
```

Agora, para escrever um arquivo CSV:

```java
import java.io.FileWriter;
import java.io.IOException;

public class CSVWriter {
    public static void main(String[] args) {
        String path = "saida.csv";
        String[] linhas = {
            "valor1,valor2,valor3",
            "valor4,valor5,valor6"
            // mais dados aqui...
        };

        try (FileWriter csvWriter = new FileWriter(path)) {
            for (String linha : linhas) {
                csvWriter.append(linha).append("\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Isso gera um arquivo `saida.csv` com os dados fornecidos.

## Deep Dive:
CSV surgiu nos primeiros dias da computação pessoal. É mais leve que formatos como JSON ou XML, mas menos estruturado. Alternativas incluem Apache Commons CSV ou bibliotecas como OpenCSV para manipulação mais robusta e complexa de CSV em Java, com melhores práticas de análise e escrita.

## Ver Também:
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
- Tutorial Java sobre FileReader: https://docs.oracle.com/javase/9/docs/api/java/io/FileReader.html
- Tutorial Java sobre FileWriter: https://docs.oracle.com/javase/9/docs/api/java/io/FileWriter.html
