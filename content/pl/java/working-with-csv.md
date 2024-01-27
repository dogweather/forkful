---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
CSV, czyli wartości oddzielone przecinkami, to format plików używany do przechowywania tabelarycznych danych. Programiści używają go z powodu prostoty, czytelności i łatwości w wymianie danych między różnymi systemami.

## Jak to zrobić:
Oto przykłady kodu w Java, jak obsłużyć CSV:

``` Java
import java.io.*;
import java.util.*;

// Czytanie z CSV
public class ReadCsvExample {
    public static void main(String[] args) {
        String path = "data.csv";
        String line = "";
        
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                // Zakładamy, że mamy 3 kolumny
                System.out.println("Kolumna 1: " + values[0] + " Kolumna 2: " + values[1] + " Kolumna 3: " + values[2]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

// Zapisywanie do CSV
public class WriteCsvExample {
    public static void main(String[] args) {
        String path = "output.csv";
        List<String[]> data = Arrays.asList(
            new String[]{"ID", "Imię", "Wiek"},
            new String[]{"1", "Jan", "22"},
            new String[]{"2", "Anna", "28"}
        );

        try (PrintWriter pw = new PrintWriter(new FileWriter(path))) {
            data.stream()
                .map(row -> String.join(",", row))
                .forEach(pw::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

## Deep Dive
CSV istnieje od lat 70., ale nadal pozostaje popularny z powodu swojej prostoty i uniwersalności. Alternatywą jest np. JSON czy XML, które są bardziej elastyczne, ale też bardziej złożone. Implementacja CSV w Javie nie wymaga dodatkowych bibliotek, lecz przy skomplikowanych przypadkach warto skorzystać np. z `Apache Commons CSV` lub `OpenCSV`.

## Zobacz także:
- Specyfikacja CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Apache Commons CSV: [https://commons.apache.org/proper/commons-csv/](https://commons.apache.org/proper/commons-csv/)
- OpenCSV: [http://opencsv.sourceforge.net/](http://opencsv.sourceforge.net/)
