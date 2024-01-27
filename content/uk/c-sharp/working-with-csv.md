---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
"Що таке та Навіщо?"
Робота з CSV (Comma-Separated Values) полягає у читанні та записі даних у текстовому форматі, де значення розділені комами. Програмісти використовують CSV через його простоту та широку підтримку у різних додатках.

## How to:
"Як це зробити:"
```C#
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        // Читання CSV файлу
        var csvLines = File.ReadAllLines("data.csv");
        foreach (var line in csvLines)
        {
            var values = line.Split(',');
            Console.WriteLine($"Name: {values[0]}, Age: {values[1]}");
        }

        // Запис у CSV файл
        var people = new List<(string Name, int Age)>
        {
            ("Olena", 28),
            ("Andriy", 35)
        };

        using (var writer = new StreamWriter("output.csv"))
        {
            foreach (var person in people)
            {
                var newLine = $"{person.Name},{person.Age}";
                writer.WriteLine(newLine);
            }
        }
    }
}
```
Sample output:
```
Name: Olena, Age: 28
Name: Andriy, Age: 35
```

## Deep Dive
"Занурення у Деталі:"
CSV стандарту не існує, але формат став популярним у 1970-х. Альтернативи — JSON і XML, які легше парсятись програмами, але CSV часто використовують через простоту. Програмісти повинні обережно обробляти дані у CSV, особливо якщо вони містять коми або переводи рядків у значеннях.

## See Also
"Дивіться також:"
- [RFC 4180](https://tools.ietf.org/html/rfc4180), опис стандарту CSV.
- [CsvHelper](https://joshclose.github.io/CsvHelper/), популярна бібліотека для роботи з CSV у C#.
- [FileHelpers](http://www.filehelpers.net/), ще одна бібліотека сильно спрощує читання та запис у CSV файли.
