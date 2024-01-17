---
title:                "Робота з csv."
html_title:           "Java: Робота з csv."
simple_title:         "Робота з csv."
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & Чому?

Робота з CSV - це процес обробки табличних даних, які зберігаються у форматі Comma-Separated Values (значення, розділені комами). Цей формат дозволяє легко зберігати та передавати великі обсяги даних. Програмісти часто працюють з CSV, оскільки він є одним з найбільш поширених форматів для обміну даними.

## Як це робити?

```Java
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import com.opencsv.CSVReader;

public class CSVReaderExample {

    public static void main(String[] args) throws IOException {

        // створюємо об'єкт CSVReader для читання файлу
        CSVReader reader = new CSVReader(new FileReader("file.csv"));

        // зберігаємо всі рядки файла у список
        List<String[]> rows = reader.readAll();

        // виводимо дані з кожного рядка
        for (String[] row : rows) {
            for (String cell : row) {
                System.out.print(cell + " ");
            }
            System.out.println();
        }
        
        // закриваємо об'єкт CSVReader
        reader.close();
    }
}
```

Виведення:

```
Name Age Gender
John 25 Male
Mary 30 Female
```

## Поглиблене дослідження

CSV є стандартом для обміну структурованими даними з 1970-х років. Існують інші альтернативи, такі як JSON і XML, але вони не є такими простими у використанні, як CSV. При роботі з CSV потрібно враховувати різні варіації формату, такі як відмінності у символах розділювачів та визначення рядків, які містять коми.

## Дивіться також

- [Офіційна документація Java для роботи з CSV](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Бібліотека OpenCSV для роботи з CSV у Java](http://opencsv.sourceforge.net/)
- [Стаття на Habr про роботу з CSV у Java](https://habr.com/ru/post/486890/)