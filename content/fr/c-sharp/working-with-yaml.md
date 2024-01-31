---
title:                "Travailler avec YAML"
date:                  2024-01-19
simple_title:         "Travailler avec YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

YAML, c'est comme JSON mais plus lisible. Les développeurs l'utilisent pour des configs, des documents, des sérialisations. Simple à écrire et à lire.

## Comment faire :
Installe YamlDotNet depuis NuGet. Utilise ça pour lire et écrire des fichiers YAML. Voici comment :

```csharp
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var ymlText = @"
name: Elon
job: Engineer
languages:
  - C#
  - Python";

        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        var person = deserializer.Deserialize<Person>(ymlText);

        Console.WriteLine($"{person.Name} is a {person.Job}.");

        var serializer = new SerializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        var newYml = serializer.Serialize(person);
        
        Console.WriteLine(newYml);
    }
}

public class Person
{
    public string Name { get; set; }
    public string Job { get; set; }
    public string[] Languages { get; set; }
}
```

Sortie :
```
Elon is an Engineer.
name: Elon
job: Engineer
languages:
- C#
- Python
```

## En Profondeur

YAML est né en 2001, pratique pour l'humain, basé sur une structure en clé-valeur. Alternatives ? JSON, XML. YAML est plus adapté pour les configs complexes.

## Voir Aussi

- YamlDotNet : [github.com/aaubry/YamlDotNet](https://github.com/aaubry/YamlDotNet)
- Spécifications YAML : [yaml.org/spec](https://yaml.org/spec/)
