---
title:                "Werken met YAML"
aliases:
- nl/c-sharp/working-with-yaml.md
date:                  2024-01-28T22:11:42.066600-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
YAML is een door mensen goed leesbare gegevensserialisatiestandaard die wordt gebruikt in programmering voor configuratiebestanden, gegevensopslag en meer. Programmeurs gebruiken het vanwege de leesbaarheid en eenvoud in complexe applicaties en systemen.

## Hoe te:
Om met YAML in C# te werken, heb je de YamlDotNet-bibliotheek nodig. Je kunt deze installeren via NuGet: `Install-Package YamlDotNet`.

Eerst, laten we een object serialiseren naar een YAML-string:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Persoon {
    public string Voornaam { get; set; }
    public string Achternaam { get; set; }
    public int Leeftijd { get; set; }
}

class Programma {
    static void Main(string[] args) {
        var persoon = new Persoon {
            Voornaam = "Jamie",
            Achternaam = "Smith",
            Leeftijd = 35
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        string yaml = serializer.Serialize(persoon);
        Console.WriteLine(yaml);
    }
}
```

Uitvoer:
```yaml
voornaam: Jamie
achternaam: Smith
leeftijd: 35
```

Vervolgens, laten we een YAML-bestand lezen en het deserialiseren:

```C#
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Programma {
    static void Main(string[] args) {
        var yaml = @"
voornaam: Jamie
achternaam: Smith
leeftijd: 35
";
        
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        
        Persoon persoon = deserializer.Deserialize<Persoon>(yaml);
        
        Console.WriteLine($"Hallo, {persoon.Voornaam} {persoon.Achternaam}!");
    }
}

public class Persoon {
    public string Voornaam { get; set; }
    public string Achternaam { get; set; }
    public int Leeftijd { get; set; }
}
```

Uitvoer:
```
Hallo, Jamie Smith!
```

## Diepgaande Duik
YAML, wat staat voor "YAML Ain't Markup Language," werd voor het eerst voorgesteld in 2001 om leesbaarder te zijn dan XML. Het wordt veel gebruikt in DevOps voor CI/CD pipeline-configuraties, zoals in Docker Compose-bestanden of Kubernetes-implementatiemanifesten. JSON is een YAML-superset, wat betekent dat JSON-bestanden ook geldige YAML zijn. Wat implementatie betreft, vereist het parsen van YAML in C# een bibliotheek zoals YamlDotNet omdat er geen native ondersteuning is.

## Zie Ook
- [YamlDotNet GitHub Repository](https://github.com/aaubry/YamlDotNet)
- [Officiële YAML website](https://yaml.org)
- [YAML Specificatie](https://yaml.org/spec/1.2/spec.html)
