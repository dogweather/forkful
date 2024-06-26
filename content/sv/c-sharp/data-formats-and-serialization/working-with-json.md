---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:14.487609-07:00
description: "Hur man g\xF6r: C# tillhandah\xE5ller namnutrymmet `System.Text.Json`\
  \ f\xF6r effektiv JSON-bearbetning. F\xF6r att parse:a en JSON-str\xE4ng till ett\
  \ C#-objekt,\u2026"
lastmod: '2024-03-13T22:44:37.934790-06:00'
model: gpt-4-0125-preview
summary: "C# tillhandah\xE5ller namnutrymmet `System.Text.Json` f\xF6r effektiv JSON-bearbetning."
title: Arbeta med JSON
weight: 38
---

## Hur man gör:


### Parse:a JSON-sträng till ett objekt
C# tillhandahåller namnutrymmet `System.Text.Json` för effektiv JSON-bearbetning. För att parse:a en JSON-sträng till ett C#-objekt, definiera en klass som matchar JSON-strukturen och använd metoden `JsonSerializer.Deserialize`.

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

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Output: Namn: John, Ålder: 30
    }
}
```

### Generera JSON från ett objekt
För att konvertera ett C#-objekt tillbaka till en JSON-sträng, använd metoden `JsonSerializer.Serialize`.

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
        // Output: {"Name":"Jane","Age":25}
    }
}
```

### Använda Newtonsoft.Json
`Newtonsoft.Json` (eller Json.NET) är ett populärt tredjepartsbibliotek som erbjuder mer flexibilitet och alternativ för JSON-serialisering och deserialisering.

För att använda Json.NET, måste du först installera `Newtonsoft.Json`-paketet via NuGet. Sedan kan du deserialisera en JSON-sträng så här:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Output: Namn: Mike, Ålder: 22
    }
}
```

För att generera JSON från ett objekt med Json.NET:

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
        // Output: {"Name":"Ella","Age":28}
    }
}
```

Dessa snuttar ger en snabbstart för att hantera JSON i C#, och visar både de inbyggda funktionerna i `System.Text.Json` och de omfattande funktionerna i `Newtonsoft.Json`.
