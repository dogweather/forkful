---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:00.832330-07:00
description: "Praca z plikami CSV obejmuje odczytywanie z nich danych oraz zapisywanie\
  \ danych do plik\xF3w o warto\u015Bciach oddzielonych przecinkami (CSV), popularnego\u2026"
lastmod: '2024-03-13T22:44:35.301180-06:00'
model: gpt-4-0125-preview
summary: "Praca z plikami CSV obejmuje odczytywanie z nich danych oraz zapisywanie\
  \ danych do plik\xF3w o warto\u015Bciach oddzielonych przecinkami (CSV), popularnego\u2026"
title: Praca z plikami CSV
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV obejmuje odczytywanie z nich danych oraz zapisywanie danych do plików o wartościach oddzielonych przecinkami (CSV), popularnego formatu wymiany danych ze względu na jego prostotę i szerokie wsparcie. Programiści manipulują plikami CSV w celu realizacji zadań takich jak import/eksport danych, analiza danych oraz wymiana informacji pomiędzy różnymi systemami.

## Jak to zrobić:

### Odczytywanie pliku CSV przy użyciu standardowej biblioteki Java

Java nie ma wbudowanego wsparcia dla CSV w swojej standardowej bibliotece, ale można łatwo odczytać plik CSV, używając klas z `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Podaj ścieżkę do pliku CSV
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Zakładając, że przecinek jest separatorem
                // Przetwarzaj dane
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

### Zapisywanie do pliku CSV przy użyciu standardowej biblioteki Java

Aby zapisać dane do pliku CSV, można użyć klas z `java.io`, takich jak `FileWriter` i `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Podaj ścieżkę do pliku CSV na wyjściu

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Zakładając, że przecinek jest separatorem
            }
            sb.deleteCharAt(sb.length() - 1); // Usuń ostatni przecinek
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Używanie biblioteki innej firmy: Apache Commons CSV

Apache Commons CSV jest popularną biblioteką do obsługi plików CSV w Javie. Znacznie upraszcza odczytywanie i zapisywanie plików CSV.

Dodaj zależność do swojego projektu:

Dla Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Sprawdź najnowszą wersję -->
</dependency>
```

#### Odczytywanie pliku CSV:

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
                // Dostęp do wartości poprzez indeksy kolumn
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

#### Zapisywanie do pliku CSV:

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"First Name", "Last Name", "Age", "City"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Rzutowanie na Object[] jest tutaj konieczne
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV automatycznie radzi sobie ze złożonościami takimi jak cytaty i przecinki w polach, co czyni ją solidnym wyborem do manipulacji CSV w Javie.
