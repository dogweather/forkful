---
title:                "Робота з CSV"
aliases:
- /uk/java/working-with-csv.md
date:                  2024-02-03T19:20:49.549935-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з CSV-файлами передбачає читання та запис даних до файлів значень, розділених комами (CSV), які є популярним форматом для обміну даними, оскільки він простий та широко підтримується. Програмісти маніпулюють CSV-файлами для завдань, таких як імпорт/експорт даних, аналіз даних та обмін інформацією між різними системами.

## Як це зробити:

### Читання файлу CSV за допомогою стандартної бібліотеки Java

Java не має вбудованої підтримки CSV у своїй стандартній бібліотеці, але ви легко можете читати CSV-файл за допомогою класів `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Вкажіть шлях до файлу CSV
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Припускаючи, що кома є роздільником
                // Обробка даних
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

### Запис до файлу CSV за допомогою стандартної бібліотеки Java

Для запису даних до CSV-файлу ви можете використовувати класи `java.io`, такі як `FileWriter` і `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Вкажіть шлях до вихідного файлу CSV

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Припускаючи, що кома є роздільником
            }
            sb.deleteCharAt(sb.length() - 1); // Видалити останню кому
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Використання сторонньої бібліотеки: Apache Commons CSV

Apache Commons CSV є популярною бібліотекою для роботи з CSV-файлами в Java. Вона значно спрощує читання та запис файлів CSV.

Додайте залежність до вашого проекту:

Для Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Перевірте останню версію -->
</dependency>
```

#### Читання файлу CSV:

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
                // Доступ до значень за індексами стовпців
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

#### Запис до файлу CSV:

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"Ім'я", "Прізвище", "Вік", "Місто"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Здійснення приведення до Object[] тут необхідно
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV автоматично вирішує складнощі, такі як кавички та коми в полях, роблячи його надійним вибором для маніпуляції CSV у Java.
