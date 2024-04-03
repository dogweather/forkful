---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:22.866362-07:00
description: 'Como Fazer: #.'
lastmod: '2024-03-13T22:44:46.479091-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Trabalhando com CSV
weight: 37
---

## Como Fazer:


### Lendo um arquivo CSV usando a biblioteca padrão Java
Java não tem suporte embutido para CSV em sua biblioteca padrão, mas você pode facilmente ler um arquivo CSV usando as classes `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Especifique o caminho para o arquivo CSV
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Supondo que uma vírgula seja o delimitador
                // Processar os dados
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Escrevendo em um arquivo CSV usando a biblioteca padrão Java
Para escrever dados em um arquivo CSV, você pode usar classes `java.io` como `FileWriter` e `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Especifique o caminho para o arquivo CSV de saída

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Supondo que uma vírgula seja o delimitador
            }
            sb.deleteCharAt(sb.length() - 1); // Remover a última vírgula
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Usando uma biblioteca de terceiros: Apache Commons CSV
Apache Commons CSV é uma biblioteca popular para manipulação de arquivos CSV em Java. Ela simplifica significativamente a leitura e escrita de arquivos CSV.

Adicione a dependência ao seu projeto:

Para Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Verifique a versão mais recente -->
</dependency>
```

#### Lendo um arquivo CSV:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        try (Reader reader = new FileReader(csvFile);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // Acessando valores pelos índices das colunas
                String columnOne = csvRecord.get(0);
                String columnTwo = csvRecord.get(1);
                System.out.println(columnOne + " " + columnTwo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### Escrevendo em um arquivo CSV:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"Primeiro Nome", "Último Nome", "Idade", "Cidade"};
        String[] data = {"John", "Doe", "30", "Nova York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // É necessário fazer o cast para Object[] aqui
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV lida com complexidades como aspas e vírgulas dentro dos campos automaticamente, tornando-a uma escolha robusta para manipulação de arquivos CSV em Java.
