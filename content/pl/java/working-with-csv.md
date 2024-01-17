---
title:                "Praca z plikami csv"
html_title:           "Java: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pracowanie z plikami CSV jest powszechnym zadaniem dla programistów Java. CSV (ang. Comma-Separated Values) jest to prosty format pliku tekstowego, który zawiera dane w formacie tabelarycznym, gdzie każda linijka odpowiada jednemu rekordowi, a pola są oddzielone przecinkami. Programiści pracują z plikami CSV, ponieważ jest to wygodny i powszechnie stosowany sposób przechowywania danych.

## Jak to zrobić:

### Wczytywanie pliku CSV:

```java
// Importujemy niezbędne klasy
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

try {
    // Tworzymy obiekt czytający plik CSV
    BufferedReader reader = new BufferedReader(new FileReader("nazwa_pliku.csv"));
    String line = "";

    // Wczytujemy kolejne linijki pliku
    while ((line = reader.readLine()) != null) {
        // Dzielimy linijkę na pola, korzystając z separatora ","
        String[] fields = line.split(",");

        // Wyświetlamy wartości pól
        System.out.println(fields[0]); // Pierwsze pole
        System.out.println(fields[1]); // Drugie pole
        // ...
    }

    // Zamykamy czytnik pliku
    reader.close();

} catch (IOException e) {
    e.printStackTrace();
}
```

### Zapisywanie do pliku CSV:

```java
// Importujemy niezbędne klasy
import java.io.FileWriter;
import java.io.IOException;

try {
    // Tworzymy obiekt zapisujący do pliku CSV
    FileWriter writer = new FileWriter("nazwa_pliku.csv");

    // Przykładowe dane do zapisania
    String[] data = {"1,Jan,Kowalski", "2,Katarzyna,Nowak"};

    // Zapisujemy każdą linijkę do pliku
    for (String line : data) {
        writer.write(line + "\n");
    }

    // Zamykamy zapisywanie do pliku
    writer.close();

} catch (IOException e) {
    e.printStackTrace();
}
```

## Głębsze zagłębienie:

### Kontekst historyczny:

Format CSV został wprowadzony przez firmę Microsoft w 1983 roku jako sposób przechowywania danych w programie arkusza kalkulacyjnego Excel. Obecnie jest powszechnie stosowany także w innych programach i systemach.

### Alternatywy:

Alternatywami dla formatu CSV są na przykład format JSON lub XML, które są bardziej strukturalne i elastyczne, ale także bardziej skomplikowane do przetwarzania dla programisty.

### Szczegóły implementacji:

W przykładach użyto klas z pakietu java.io, jednak istnieją także inne biblioteki oferujące wygodniejsze sposoby wczytywania i zapisywania danych w formacie CSV, takie jak Apache Commons CSV czy OpenCSV.

## Zobacz także:

- Dokumentacja klasy BufferedReader: https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html
- Dokumentacja klasy FileWriter: https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html
- Biblioteka Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- Biblioteka OpenCSV: http://opencsv.sourceforge.net/