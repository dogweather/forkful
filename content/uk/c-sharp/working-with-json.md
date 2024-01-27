---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
JSON - формат для обміну даними. Програмісти вживають JSON, бо він легкий та сумісний із багатьма мовами програмування.

## How to: (Як це зробити:)
```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"name\":\"Oleksiy\",\"age\":30}";
        
        // Deserialize JSON to object
        Person oleksiy = JsonSerializer.Deserialize<Person>(jsonString);
        Console.WriteLine($"{oleksiy.Name} - {oleksiy.Age}");

        oleksiy.Age += 1; // Happy Birthday, Oleksiy!

        // Serialize object to JSON
        string updatedJsonString = JsonSerializer.Serialize(oleksiy);
        Console.WriteLine(updatedJsonString);
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}
```
Output:
```
Oleksiy - 30
{"name":"Oleksiy","age":31}
```

## Deep Dive (Занурення в глибину)
JSON, або JavaScript Object Notation, з'явився в початку 2000-х, як легкий формат обміну даними. Альтернативи включають XML і YAML. C# використовує System.Text.Json для роботи з JSON, що є швидшим та ефективнішим порівняно з Newtonsoft.Json, хоча останній був стандартом де-факто багато років.

## See Also (Дивіться також)
- [Офіційну документацію System.Text.Json](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-overview)
- [JSON на Вікіпедії](https://uk.wikipedia.org/wiki/JSON)
