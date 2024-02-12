---
title:                "Arbeta med CSV"
aliases:
- /sv/java/working-with-csv.md
date:                  2024-02-03T19:20:31.957562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV-filer innebär att läsa från och skriva data till kommaseparerade värden (CSV) filer, ett populärt format för datautbyte eftersom det är enkelt och brett understött. Programmerare manipulerar CSV-filer för uppgifter såsom dataimport/export, dataanalys och informationsutbyte mellan olika system.

## Hur man gör:

### Läsa en CSV-fil med hjälp av standard Java-biblioteket

Java har inte inbyggt stöd för CSV i sitt standardbibliotek, men du kan enkelt läsa en CSV-fil med hjälp av klasser i `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Ange sökvägen till CSV-filen
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Antagandes att ett komma är avgränsaren
                // Bearbeta datan
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

### Skriva till en CSV-fil med hjälp av standard Java-biblioteket

För att skriva data till en CSV-fil kan du använda `java.io`-klasser såsom `FileWriter` och `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Ange sökvägen till utdata CSV-filen

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Antagandes att ett komma är avgränsaren
            }
            sb.deleteCharAt(sb.length() - 1); // Ta bort det sista kommat
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Använda ett bibliotek från tredje part: Apache Commons CSV

Apache Commons CSV är ett populärt bibliotek för hantering av CSV-filer i Java. Det förenklar avsevärt läsning och skrivning av CSV-filer.

Lägg till beroendet i ditt projekt:

För Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Kontrollera för den senaste versionen -->
</dependency>
```

#### Läsa en CSV-fil:

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
                // Åtkomst till värden genom kolumnernas index
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

#### Skriva till en CSV-fil:

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"Förnamn", "Efternamn", "Ålder", "Stad"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Gjutning till Object[] är nödvändig här
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV hanterar komplexiteter såsom citattecken och komman inom fält automatiskt, vilket gör det till ett robust val för CSV-manipulation i Java.
