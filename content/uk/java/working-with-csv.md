---
title:                "Java: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

CSV (Comma Separated Values) - це один з найбільш поширених форматів для зберігання і обміну даними. Робота з CSV є необхідною для багатьох програмістів, тому що це простий і зручний спосіб зберігати дані у табличному вигляді.

## Як працювати з CSV в Java

Найпростішим способом прочитати CSV файл в Java є використання бібліотеки OpenCSV. Для цього необхідно додати залежність у файл `pom.xml` вашого проекту:

```Java
<dependency>
  <groupId>com.opencsv</groupId>
  <artifactId>opencsv</artifactId>
  <version>5.3</version>
</dependency>
```

Після цього можна створити об'єкт `CSVReader` і використовувати його для читання файлу:

```Java
CSVReader reader = new CSVReader(new FileReader("file.csv"));
String[] nextRecord;

while ((nextRecord = reader.readNext()) != null) {
  // обробляємо рядок з даними
}

reader.close();
```

Використовуючи метод `readAll()`, можна прочитати усі рядки з файлу одразу і отримати їх у вигляді `List<String[]>`:

```Java
CSVReader reader = new CSVReader(new FileReader("file.csv"));
List<String[]> rows = reader.readAll();

for (String[] row : rows) {
  // обробляємо рядок з даними
}

reader.close();
```

Також OpenCSV дозволяє записувати дані в CSV файл за допомогою класу `CSVWriter`:

```Java
CSVWriter writer = new CSVWriter(new FileWriter("file.csv"));
// записуємо рядок з даними
String[] data = {"John Doe", "john.doe@example.com", "25"};
writer.writeNext(data);

writer.close();
```

## Глибше вивчення

Бібліотека OpenCSV надає також інші функціональні можливості, такі як вказання символу-роздільника, комплексне читання/запис даних з об'єктів та інше. Детальну інформацію про всі можливості бібліотеки можна знайти на її [офіційному сайті](http://opencsv.sourceforge.net/).

## Дивись також

- [Офіційна документація Java по роботі з CSV](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Стаття про роботу з CSV в Python](https://www.datacamp.com/community/tutorials/pandas-read-csv)