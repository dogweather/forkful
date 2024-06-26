---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:32.908190-07:00
description: ''
lastmod: '2024-04-05T22:50:58.571426-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как


### Чтение файлов CSV
```C#
using System;
using System.IO;

class ReadCSVExample
{
    static void Main()
    {
        string path = "data.csv";
        using (var reader = new StreamReader(path))
        {
            while (!reader.EndOfStream)
            {
                var line = reader.ReadLine();
                var values = line.Split(',');
                // Теперь сделайте что-то со значениями, например, напечатайте их
                Console.WriteLine(String.Join(" | ", values));
            }
        }
    }
}
```
**Пример вывода:**
```
John | Doe | johndoe@example.com
Jane | Smith | janesmith@example.com
```

### Запись файлов CSV
```C#
using System;
using System.IO;

class WriteCSVExample
{
    static void Main()
    {
        string path = "output.csv";
        var records = new[]
        {
            new[] {"Name", "Age", "Email"},
            new[] {"Alice", "23", "alice@example.com"},
            new[] {"Bob", "30", "bob@example.com"}
        };

        using (var writer = new StreamWriter(path))
        {
            foreach (var record in records)
            {
                var line = String.Join(",", record);
                writer.WriteLine(line);
            }
        }
        Console.WriteLine($"Данные записаны в {path}");
    }
}
```
**Пример вывода:**
```
Данные записаны в output.csv
```

## Углубление
CSV существует с начала времен вычислений, служа мостом между разными системами. Он не идеален — не имеет стандартной кодировки символов и плохо поддерживает многострочные поля без надежного парсера. Здесь на сцену выходят форматы вроде JSON и XML, предлагая больше сложности, но лучшую структуру для иерархических данных.

Под капотом вы обычно манипулируете строками, либо с использованием встроенных методов `string`, либо библиотеки вроде `CsvHelper` могут добавить дополнительную мощь вашей работе с CSV, предоставляя больше функций и изящно обрабатывая крайние случаи. Помните, в .NET нет встроенной обработки CSV, так что вы остаетесь наедине с манипуляциями со строками или можете выбрать стороннюю библиотеку.

## Смотрите также
Для более глубокого изучения манипуляции с CSV в C#:
- [Библиотека CsvHelper](https://joshclose.github.io/CsvHelper/)
- [Документация Microsoft по `StreamReader`](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)

Узнайте больше о альтернативах CSV:
- [Понимание JSON](https://www.json.org/json-en.html)
- [XML в двух словах](https://www.w3schools.com/xml/xml_whatis.asp)
