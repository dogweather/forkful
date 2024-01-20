---
title:                "Робота з CSV"
html_title:           "C#: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Що і навіщо? 
Робота з CSV - це зберігання та обробка даних у вигляді таблиці, де рядки представляють записи, а стовпці - поля цих записів. Це популярний формат, що використовується для обміну даними між програмами. Програмісти користуються CSV для зручної та легкої обробки та збереження структурованих даних.

## Як? 
Нижче наведені кодові приклади та вихідні дані для роботи з CSV у C#. 

```C#
// Читання CSV файлу та виведення даних
using System;
using System.IO;

var csvString = File.ReadAllText("file.csv");
Console.WriteLine(csvString);

// Запис даних у CSV файл
using System;
using System.IO;

var data = "Name, Age, City\nAnna, 25, Kyiv\nIvan, 30, Lviv";

File.WriteAllText("newFile.csv", data);

// Виведення певного стовпця з CSV файла
using System;
using System.IO;
using System.Linq;

var csvString = File.ReadAllLines("file.csv");
var cityColumn = csvString.Select(line => line.Split(',')[2]);

foreach (var city in cityColumn)
{
    Console.WriteLine(city);
}

// Повернення CSV таблиці у вигляді двовимірного масиву
using System;
using System.IO;

var csvString = File.ReadAllLines("file.csv");
var table = new string[csvString.Length, csvString[0].Split(',').Length];

for (int i = 0; i < csvString.Length; i++)
{
    for (int j = 0; j < csvString[i].Split(',').Length; j++)
    {
        table[i, j] = csvString[i].Split(',')[j];
    }
}

Console.WriteLine(table[1, 2]); // Виведе дані з другого рядка та третього стовпця
```

## Глибокий погляд
Формат CSV був вперше запропонований у 1972 році та з тих пір став стандартом для обміну даними. Незважаючи на це, існують інші формати, такі як JSON та XML, які також широко використовуються для роботи з даними. Використання кожного з цих форматів залежить від потреб та вимог проекту. Робота з CSV у C# досить проста за допомогою вбудованих методів для роботи з файлами та рядками.

## Дивись також 
Для детальнішої інформації про формат CSV та роботу з ним у C#, рекомендуємо переглянути наступні ресурси: 
- [Стаття на CodeProject про роботу з CSV у C#](https://www.codeproject.com/Articles/30705/C-CSV-Reader)