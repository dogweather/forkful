---
title:                "Arbeiten mit YAML"
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?

YAML ist ein datenorientiertes Format für Konfigurationsdateien. Programmierer nutzen es, weil es menschenlesbar und leicht zu schreiben ist und sich nahtlos in viele Entwicklungsumgebungen integriert.

## How to:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string[] Hobbies { get; set; }
}

class Program
{
    static void Main()
    {
        var ymlString = @"
        name: Max Mustermann
        age: 30
        hobbies:
          - Fußball
          - Programmieren
        ";

        var deserializer = new DeserializerBuilder().WithNamingConvention(CamelCaseNamingConvention.Instance).Build();
        var person = deserializer.Deserialize<Person>(ymlString);

        Console.WriteLine($"Name: {person.Name}, Alter: {person.Age}");
        foreach(var hobby in person.Hobbies)
        {
            Console.WriteLine($"Hobby: {hobby}");
        }
    }
}
```

Ausgabe:

```
Name: Max Mustermann, Alter: 30
Hobby: Fußball
Hobby: Programmieren
```

## Deep Dive

YAML begann um 2001 und steht für "YAML Ain't Markup Language". Es ist eine Alternative zu XML und JSON, die oft für Konfigurationsdateien genutzt wird. YAML hebt sich durch die Verwendung von Whitespace zur Datenstrukturierung ab, im Gegensatz zu den geschweiften Klammern bei JSON oder den Tags in XML. C# implementiert die Verarbeitung von YAML primär durch Drittanbieter-Bibliotheken wie YamlDotNet, da es keine native Unterstützung gibt.

## See Also

- YamlDotNet GitHub Repository: https://github.com/aaubry/YamlDotNet
- YAML offizielle Webseite: https://yaml.org
- Microsoft Konfigurationsdokumentation: https://docs.microsoft.com/de-de/aspnet/core/fundamentals/configuration/
