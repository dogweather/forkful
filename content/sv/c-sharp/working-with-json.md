---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON står för JavaScript Object Notation. Det är ett format för att lagra och utbyta data. Programmerare använder JSON eftersom det är lättläst för både människor och maskiner, och det är språköverskridande.

## How to:
I C# hanterar vi JSON med `System.Text.Json`. Här är hur du deserialiserar och serialiserar enkelt.

```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // JSON string
        string jsonString = "{\"name\":\"Anna\",\"age\":28}";

        // Deserialisera JSON till ett objekt
        Person person = JsonSerializer.Deserialize<Person>(jsonString);
        Console.WriteLine($"Namn: {person.Name}, Ålder: {person.Age}");

        // Serialisera objekt till JSON
        string serializedJson = JsonSerializer.Serialize(person);
        Console.WriteLine(serializedJson);
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
Namn: Anna, Ålder: 28
{"name":"Anna","age":28}
```

## Deep Dive
JSON har sitt ursprung i JavaScript, men dess enkelhet har gjort det till en webbstandard bortom JavaScripts gränser. Alternativ till JSON inkluderar XML och YAML. Medan `System.Text.Json` är standard i .NET Core och framåt, användes `Newtonsoft.Json` mycket förr. `System.Text.Json` fokuserar på prestanda och kompatibilitet med standarden.

## See Also
- [Microsoft's JSON documentation](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-overview)
- [JSON.org](https://www.json.org/json-en.html)
