---
aliases:
- /uk/java/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:49.549935-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV-\u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454\
  \ \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\u043F\u0438\
  \u0441 \u0434\u0430\u043D\u0438\u0445 \u0434\u043E \u0444\u0430\u0439\u043B\u0456\
  \u0432 \u0437\u043D\u0430\u0447\u0435\u043D\u044C, \u0440\u043E\u0437\u0434\u0456\
  \u043B\u0435\u043D\u0438\u0445 \u043A\u043E\u043C\u0430\u043C\u0438 (CSV), \u044F\
  \u043A\u0456 \u0454 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u043C\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0434\u043B\u044F \u043E\u0431\
  \u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u043E\u0441\u043A\
  \u0456\u043B\u044C\u043A\u0438\u2026"
lastmod: 2024-02-18 23:09:00.159060
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV-\u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454\
  \ \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0430 \u0437\u0430\u043F\u0438\
  \u0441 \u0434\u0430\u043D\u0438\u0445 \u0434\u043E \u0444\u0430\u0439\u043B\u0456\
  \u0432 \u0437\u043D\u0430\u0447\u0435\u043D\u044C, \u0440\u043E\u0437\u0434\u0456\
  \u043B\u0435\u043D\u0438\u0445 \u043A\u043E\u043C\u0430\u043C\u0438 (CSV), \u044F\
  \u043A\u0456 \u0454 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u043C\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0434\u043B\u044F \u043E\u0431\
  \u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u043E\u0441\u043A\
  \u0456\u043B\u044C\u043A\u0438\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
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
