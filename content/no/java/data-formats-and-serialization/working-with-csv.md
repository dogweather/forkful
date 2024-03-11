---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:30.137566-07:00
description: "\xC5 jobbe med CSV-filer involverer lesing fra og skriving av data til\
  \ komma-separerte verdier (CSV)-filer, et popul\xE6rt format for datautveksling\
  \ fordi det\u2026"
lastmod: '2024-03-11T00:14:14.233164-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV-filer involverer lesing fra og skriving av data til komma-separerte\
  \ verdier (CSV)-filer, et popul\xE6rt format for datautveksling fordi det\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med CSV-filer involverer lesing fra og skriving av data til komma-separerte verdier (CSV)-filer, et populært format for datautveksling fordi det er enkelt og bredt støttet. Programmerere manipulerer CSV-filer for oppgaver som dataimport/eksport, dataanalyse, og informasjonsdeling mellom ulike systemer.

## Hvordan:

### Lese en CSV-fil ved bruk av standard Java-bibliotek

Java har ikke innebygd støtte for CSV i sitt standardbibliotek, men du kan enkelt lese en CSV-fil ved å bruke `java.io`-klasser.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Angi stien til CSV-filen
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Anta at et komma er skilletegnet
                // Behandle dataene
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

### Skrive til en CSV-fil ved bruk av standard Java-bibliotek

For å skrive data til en CSV-fil, kan du bruke `java.io`-klasser som `FileWriter` og `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Angi stien til utdata-CSV-filen

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Anta at et komma er skilletegnet
            }
            sb.deleteCharAt(sb.length() - 1); // Fjern det siste kommaet
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Bruke et tredjepartsbibliotek: Apache Commons CSV

Apache Commons CSV er et populært bibliotek for håndtering av CSV-filer i Java. Det forenkler betydelig lesing og skriving av CSV-filer.

Legg til avhengigheten i prosjektet ditt:

For Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Sjekk for den nyeste versjonen -->
</dependency>
```

#### Lese en CSV-fil:

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
                // Tilgang til verdier ved indeksene til kolonnene
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

#### Skrive til en CSV-fil:

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"Fornavn", "Etternavn", "Alder", "By"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Støping til Object[] er nødvendig her
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV håndterer kompleksiteter som anførselstegn og kommaer innen felt automatisk, og gjør det til et robust valg for CSV-manipulering i Java.
