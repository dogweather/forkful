---
title:                "Робота з csv"
html_title:           "Java: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Навіщо

Цікавитесь програмуванням і хочете дізнатися більше про роботу з CSV-файлами? Java пропонує потужні інструменти для цього. Дізнайтеся, як працювати з CSV у цій статті!

## Як працювати з CSV у Java

Започнемо з імпорту необхідних класів:
```Java
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
```

Створимо об'єкт для читання файлу та зчитаємо його вміст:
```Java
BufferedReader reader = new BufferedReader(new FileReader("file.csv"));
String line;
while ((line = reader.readLine()) != null) {
    // код для обробки рядків
}
```

Розділимо рядок на окремі значення, використовуючи роздільник коми:
```Java
String[] values = line.split(",");
```

Строкові значення можна перетворити на числові:
```Java
int num = Integer.parseInt(values[0]); // перший стовпець
float dec = Float.parseFloat(values[1]); // другий стовпець
```

## Глибоке дослідження

Для роботи з потоками вводу-виводу у Java використовується пакет java.io. В ньому є класи, що допомагають читати і записувати дані у CSV-форматі. Для зручності також існують пакети, які дозволяють працювати з CSV у вигляді об'єктів, що знаходяться в пам'яті комп'ютера.

## Дивіться також

- [Офіційна документація Java](https://docs.oracle.com/en/java/javase/14/docs/api/index.html)
- [Покроковий підручник з роботи з CSV в Java](https://www.baeldung.com/java-csv-file-array)