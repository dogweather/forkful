---
title:                "Робота з CSV"
aliases:
- /uk/c-sharp/working-with-csv/
date:                  2024-02-03T19:19:40.797241-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
CSV файли (значення, розділені комами) є поширеним форматом обміну даними, який представляє табличні дані в простому тексті, використовуючи коми для розділення окремих значень. Програмісти працюють з CSV файлами для імпорту, експорту та маніпулювання даними з легкістю між різними програмами та сервісами, оскільки це простий, широко підтримуваний формат, сумісний з програмами для роботи з електронними таблицями, базами даних та мовами програмування.

## Як це зробити:
Робота з CSV файлами в C# може бути здійснена через простір імен `System.IO` для базових операцій, а для більш складних маніпуляцій або для обробки великих файлів без перешкод, можна використовувати сторонні бібліотеки, такі як `CsvHelper`. Нижче наведено приклади, як читати з файлів CSV та записувати в них, використовуючи обидва підходи.

### Читання файлу CSV за допомогою System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"шлях\до\вашого\файлу.csv";
        // Читання всіх рядків файлу CSV
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Перша колонка: {rowData[0]}, Друга колонка: {rowData[1]}");
        }
    }
}
```

**Приклад виводу:**
```
Перша колонка: Ім'я, Друга колонка: Вік
Перша колонка: Джон Доу, Друга колонка: 30
```

### Запис в файл CSV за допомогою System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"шлях\до\вашого\вихідного.csv";
        var lines = new List<string>
        {
            "Ім'я,Вік",
            "Джон Доу,30",
            "Джейн Сміт,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("Файл CSV записано.");
    }
}
```

**Приклад виводу:**
```
Файл CSV записано.
```

### Використання CsvHelper для читання CSV
Щоб використовувати CsvHelper, спочатку додайте пакет `CsvHelper` до вашого проекту за допомогою менеджера пакетів NuGet.

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"шлях\до\вашого\файлу.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Перша колонка: {record.Name}, Друга колонка: {record.Age}");
            }
        }
    }
}
```

**Приклад виводу:**
```
Перша колонка: Джон Доу, Друга колонка: 30
Перша колонка: Джейн Сміт, Друга колонка: 25
```

### Використання CsvHelper для запису CSV
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"шлях\до\вашого\вихідного.csv";
        var records = new List<Person>
        {
            new Person { Name = "Джон Доу", Age = 30 },
            new Person { Name = "Джейн Сміт", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("Файл CSV записано за допомогою CsvHelper.");
    }
}
```

**Приклад виводу:**
```
Файл CSV записано за допомогою CsvHelper.
```
