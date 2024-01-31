---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? - Was & Warum?

Arbeiten mit JSON bedeutet, Daten im JavaScript Object Notation-Format zu lesen, zu schreiben und zu parsen. Programmierer nutzen JSON wegen seines leichten und menschenlesbaren Formats für Datenübertragungen zwischen Server und Webanwendungen sowie für Konfigurationen.

## How to: - Wie geht das?

```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // JSON-String
        string jsonString = "{\"name\":\"Max\",\"age\":25}";

        // Deserialisieren in ein C#-Objekt
        Person person = JsonSerializer.Deserialize<Person>(jsonString);
        Console.WriteLine($"{person.Name}, {person.Age} Jahre alt");

        // Serialisieren eines C#-Objekts zurück in JSON
        string jsonOutput = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonOutput);
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}
```

Ausgabe:
```
Max, 25 Jahre alt
{"Name":"Max","Age":25}
```

## Deep Dive - Vertiefung

JSON, eingeführt 2001, stammt aus JavaScript, wird aber sprachübergreifend genutzt. Alternativen wie XML existieren, aber JSON ist populär für seine Simplizität und Effizienz bei Web APIs. C#-Implementierung erfolgt durch die `System.Text.Json`-Bibliothek oder Drittanbieter wie `Newtonsoft.Json`. JSON ist ideal für Datenstrukturen, bei der Arbeit mit REST APIs, und wo Leistung zählt.

## See Also - Siehe Auch

- [System.Text.Json-Dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.text.json?view=net-6.0)
- [Newtonsoft.Json-Dokumentation](https://www.newtonsoft.com/json)
- [JSON-Standard-Website](https://www.json.org/json-de.html)
