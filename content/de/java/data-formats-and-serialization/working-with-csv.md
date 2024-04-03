---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:19.525507-07:00
description: "Die Arbeit mit CSV-Dateien umfasst das Lesen von und das Schreiben in\
  \ Dateien mit kommaseparierten Werten (CSV), einem beliebten Format f\xFCr den\u2026"
lastmod: '2024-03-13T22:44:53.784736-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit CSV-Dateien umfasst das Lesen von und das Schreiben in Dateien\
  \ mit kommaseparierten Werten (CSV), einem beliebten Format f\xFCr den Datenaustausch,\
  \ da es einfach und weit verbreitet unterst\xFCtzt wird."
title: Arbeiten mit CSV
weight: 37
---

## Wie:


### Eine CSV-Datei mit der Standard-Java-Bibliothek lesen
Java hat keine integrierte Unterstützung für CSV in seiner Standardbibliothek, aber Sie können eine CSV-Datei problemlos unter Verwendung von `java.io`-Klassen lesen.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Pfad zur CSV-Datei angeben
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Annahme, dass ein Komma der Trenner ist
                // Die Daten verarbeiten
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

### In eine CSV-Datei mit der Standard-Java-Bibliothek schreiben
Um Daten in eine CSV-Datei zu schreiben, können Sie `java.io`-Klassen wie `FileWriter` und `BufferedWriter` verwenden.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Pfad zur Ausgabe-CSV-Datei angeben

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Annahme, dass ein Komma der Trenner ist
            }
            sb.deleteCharAt(sb.length() - 1); // Das letzte Komma entfernen
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Verwendung einer Drittanbieterbibliothek: Apache Commons CSV
Apache Commons CSV ist eine beliebte Bibliothek zur Bearbeitung von CSV-Dateien in Java. Sie vereinfacht das Lesen und Schreiben von CSV-Dateien erheblich.

Fügen Sie die Abhängigkeit zu Ihrem Projekt hinzu:

Für Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Aktuellste Version überprüfen -->
</dependency>
```

#### Eine CSV-Datei lesen:
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
                // Zugriff auf Werte durch die Indizes der Spalten
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

#### In eine CSV-Datei schreiben:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"Vorname", "Nachname", "Alter", "Stadt"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Das Casting zu Object[] ist hier notwendig
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV behandelt Komplexitäten wie Anführungszeichen und Kommas innerhalb von Feldern automatisch, was es zu einer robusten Wahl für die CSV-Manipulation in Java macht.
