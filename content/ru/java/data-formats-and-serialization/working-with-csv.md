---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:49.446275-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV, \u0447\u0442\u043E\
  \ \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0437\u043D\u0430\u0447\u0435\
  \u043D\u0438\u044F, \u0440\u0430\u0437\u0434\u0435\u043B\u0451\u043D\u043D\u044B\
  \u0435 \u0437\u0430\u043F\u044F\u0442\u044B\u043C\u0438 (Comma-Separated Values),\
  \ \u043F\u043E\u0434\u0440\u0430\u0437\u0443\u043C\u0435\u0432\u0430\u0435\u0442\
  \ \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0443 \u0434\u0430\u043D\u043D\
  \u044B\u0445 \u0432 \u0444\u043E\u0440\u043C\u0430\u0442\u0435 \u043F\u0440\u043E\
  \u0441\u0442\u043E\u0433\u043E \u0442\u0435\u043A\u0441\u0442\u0430, \u0433\u0434\
  \u0435 \u043A\u0430\u0436\u0434\u0430\u044F\u2026"
lastmod: '2024-03-13T22:44:44.864241-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV, \u0447\u0442\u043E \u043E\
  \u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0437\u043D\u0430\u0447\u0435\u043D\u0438\
  \u044F, \u0440\u0430\u0437\u0434\u0435\u043B\u0451\u043D\u043D\u044B\u0435 \u0437\
  \u0430\u043F\u044F\u0442\u044B\u043C\u0438 (Comma-Separated Values), \u043F\u043E\
  \u0434\u0440\u0430\u0437\u0443\u043C\u0435\u0432\u0430\u0435\u0442 \u043E\u0431\u0440\
  \u0430\u0431\u043E\u0442\u043A\u0443 \u0434\u0430\u043D\u043D\u044B\u0445 \u0432\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u0435 \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0433\u043E \u0442\u0435\u043A\u0441\u0442\u0430, \u0433\u0434\u0435 \u043A\u0430\
  \u0436\u0434\u0430\u044F\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Что и Почему?

Работа с CSV, что означает значения, разделённые запятыми (Comma-Separated Values), подразумевает обработку данных в формате простого текста, где каждая строка - это запись данных с полями, разделёнными запятыми. Программисты выбирают CSV, потому что они просты в использовании, широко поддерживаются и идеально подходят для обмена данными между различными приложениями.

## Как:

Давайте прочитаем и запишем файлы CSV в Java, используя широко используемую библиотеку `OpenCSV`. Сначала добавьте зависимость в ваш `pom.xml`, если вы используете Maven.

```xml
<dependency>
    <groupId>com.opencsv</groupId>
    <artifactId>opencsv</artifactId>
    <version>5.6</version> <!-- Проверьте последнюю версию -->
</dependency>
```

### Запись файла CSV

```java
import com.opencsv.CSVWriter;
import java.io.FileWriter;
import java.io.IOException;

public class CSVWritingExample {
    public static void main(String[] args) {
        String[] заголовок = {"Имя", "Возраст", "Страна"};
        String[] запись1 = {"Алиса", "24", "США"};
        String[] запись2 = {"Боб", "19", "Канада"};

        try (CSVWriter writer = new CSVWriter(new FileWriter("data.csv"))) {
            writer.writeNext(заголовок);
            writer.writeNext(запись1);
            writer.writeNext(запись2);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Чтение файла CSV

```java
import com.opencsv.CSVReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class CSVReadingExample {
    public static void main(String[] args) {

        try (CSVReader reader = new CSVReader(new FileReader("data.csv"))) {
            List<String[]> r = reader.readAll();
            r.forEach(x -> System.out.println(x[0] + ", " + x[1] + ", " + x[2]));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Пример вывода после чтения:

```
Имя, Возраст, Страна
Алиса, 24, США
Боб, 19, Канада
```

## Глубже

Исторически файлы CSV использовались ещё с первых дней персональных компьютеров, что делает их своего рода лингва франка для обмена данными. Такие альтернативы, как JSON, XML или даже форматы Excel, могут предложить более продвинутые функции, но простота CSV обеспечивает его выживание. При работе с Java, хотя `OpenCSV` является популярным выбором, вы также можете использовать `java.util.Scanner` или `java.io.BufferedReader` для очень базовых задач, хотя вам придется заниматься разбором самостоятельно. `Apache Commons CSV` - это еще одна мощная библиотека, доступная для аналогичных задач.

## См. также

- Домашняя страница OpenCSV для документации и руководств: http://opencsv.sourceforge.net/
- Apache Commons CSV для альтернативного подхода: https://commons.apache.org/proper/commons-csv/
- Официальные учебные пособия Java от Oracle по операциям ввода-вывода: https://docs.oracle.com/javase/tutorial/essential/io/
