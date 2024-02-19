---
aliases:
- /it/java/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:34.667076-07:00
description: "Lavorare con file CSV comporta la lettura da e la scrittura su file\
  \ di Valori Separati da Virgola (CSV), un formato popolare per lo scambio di dati\
  \ perch\xE9\u2026"
lastmod: 2024-02-18 23:08:55.787418
model: gpt-4-0125-preview
summary: "Lavorare con file CSV comporta la lettura da e la scrittura su file di Valori\
  \ Separati da Virgola (CSV), un formato popolare per lo scambio di dati perch\xE9\
  \u2026"
title: Lavorare con i CSV
---

{{< edit_this_page >}}

## Cosa e Perché?

Lavorare con file CSV comporta la lettura da e la scrittura su file di Valori Separati da Virgola (CSV), un formato popolare per lo scambio di dati perché è semplice e ampiamente supportato. I programmatori manipolano file CSV per compiti come l'importazione/esportazione di dati, l'analisi dei dati e la condivisione di informazioni tra diversi sistemi.

## Come fare:

### Leggere un file CSV utilizzando la libreria standard Java

Java non ha un supporto integrato per i CSV nella sua libreria standard, ma è possibile leggere facilmente un file CSV utilizzando le classi `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Specificare il percorso del file CSV
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Supponendo che una virgola sia il delimitatore
                // Elaborare i dati
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

### Scrivere su un file CSV utilizzando la libreria standard Java

Per scrivere dati su un file CSV, è possibile utilizzare classi `java.io` come `FileWriter` e `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Specificare il percorso del file CSV di output

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Supponendo che una virgola sia il delimitatore
            }
            sb.deleteCharAt(sb.length() - 1); // Rimuovere l'ultima virgola
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Utilizzo di una libreria di terze parti: Apache Commons CSV

Apache Commons CSV è una libreria popolare per la gestione dei file CSV in Java. Semplifica notevolmente la lettura e la scrittura di file CSV.

Aggiungi la dipendenza al tuo progetto:

Per Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Verifica la versione più recente -->
</dependency>
```

#### Leggere un file CSV:

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
                // Accedere ai valori tramite gli indici delle colonne
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

#### Scrivere su un file CSV:

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"Nome", "Cognome", "Età", "Città"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Qui è necessario il casting a Object[]
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV gestisce automaticamente le complessità come le virgolette e le virgole all'interno dei campi, rendendolo una scelta robusta per la manipolazione di CSV in Java.
