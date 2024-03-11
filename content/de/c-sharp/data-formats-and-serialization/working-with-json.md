---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:13.741004-07:00
description: "Die Arbeit mit JSON (JavaScript Object Notation) umfasst das Parsen,\
  \ Generieren und Abfragen von JSON-Daten und ist somit eine entscheidende F\xE4\
  higkeit f\xFCr\u2026"
lastmod: '2024-03-11T00:14:27.806046-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit JSON (JavaScript Object Notation) umfasst das Parsen, Generieren\
  \ und Abfragen von JSON-Daten und ist somit eine entscheidende F\xE4higkeit f\xFC\
  r\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit JSON (JavaScript Object Notation) umfasst das Parsen, Generieren und Abfragen von JSON-Daten und ist somit eine entscheidende Fähigkeit für die moderne Programmierung. Dieses Daten-Austauschformat wird aufgrund seiner leichten Lesbarkeit und Sprachunabhängigkeit zunehmend in Webdiensten und APIs verwendet, was es für C#-Programmierer, die an vernetzten Anwendungen arbeiten oder mit webbasierten Daten interagieren, unerlässlich macht.

## Wie geht das:

### Parsen eines JSON-Strings zu einem Objekt

C# bietet den Namespace `System.Text.Json` für effiziente JSON-Verarbeitung an. Um einen JSON-String in ein C#-Objekt zu parsen, definieren Sie eine Klasse, die der JSON-Struktur entspricht, und verwenden Sie die Methode `JsonSerializer.Deserialize`.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Alter: {person.Age}");
        // Ausgabe: Name: John, Alter: 30
    }
}
```

### Generieren eines JSON aus einem Objekt

Um ein C#-Objekt wieder in einen JSON-String umzuwandeln, verwenden Sie die Methode `JsonSerializer.Serialize`.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // Ausgabe: {"Name":"Jane","Age":25}
    }
}
```

### Verwendung von Newtonsoft.Json

`Newtonsoft.Json` (oder Json.NET) ist eine beliebte Drittanbieter-Bibliothek, die mehr Flexibilität und Optionen für die JSON-Serialisierung und -Deserialisierung bietet.

Um Json.NET zu verwenden, müssen Sie zuerst das Paket `Newtonsoft.Json` über NuGet installieren. Dann können Sie so einen JSON-String deserialisieren:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Alter: {person.Age}");
        // Ausgabe: Name: Mike, Alter: 22
    }
}
```

Für das Generieren eines JSON aus einem Objekt mit Json.NET:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // Ausgabe: {"Name":"Ella","Age":28}
    }
}
```

Diese Schnipsel bieten einen schnellen Einstieg in die Handhabung von JSON in C#, und demonstrieren sowohl die integrierten Fähigkeiten von `System.Text.Json` als auch die umfangreichen Funktionen von `Newtonsoft.Json`.
