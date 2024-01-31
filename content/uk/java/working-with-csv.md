---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Робота з CSV – це читання та запис даних у форматі з розділенням комами. Програмісти використовують CSV через його простоту і універсальність для обміну даними.

## How to: (Як це зробити:)
Читання з CSV файлу:
```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CSVReader {
    public static void main(String[] args) {
        String path = "data.csv";
        String line;
        
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                // Щось робимо з даними
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Запис у CSV файл:
```java
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Arrays;

public class CSVWriter {
    public static void main(String[] args) {
        List<List<String>> data = Arrays.asList(
                Arrays.asList("Ім'я", "Ел. пошта", "Телефон"),
                Arrays.asList("Андрій", "andriy@example.com", "123-456-7890"),
                Arrays.asList("Юлія", "yulia@example.com", "098-765-4321")
        );
        String path = "output.csv";
        
        try (FileWriter csvWriter = new FileWriter(path)) {
            for (List<String> rowData : data) {
                csvWriter.append(String.join(",", rowData));
                csvWriter.append("\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

## Deep Dive (Занурення глибше)
CSV (Comma-Separated Values) з’явилося у 1970-х і досі популярне для табличних даних. Є альтернативи, як-от JSON чи XML, але CSV виграє простотою. При роботі з CSV треба пам’ятати про екранування ком, нових рядків та лапок, щоб уникнути помилок у файлах.

## See Also (Додатково)
- [RFC 4180](https://tools.ietf.org/html/rfc4180) - формальний стандарт CSV.
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/) - бібліотека для роботи з CSV в Java.
- [OpenCSV](http://opencsv.sourceforge.net/) - ще одна бібліотека Java для CSV.

*Примітка: Це скорочена стаття, без зайвих деталей і припущень. Озирніться за теорією та практикою для повного розуміння.*
