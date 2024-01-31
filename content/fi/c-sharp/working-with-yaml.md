---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
YAML on ihmisen luettava datan serialisointiformaatti konfiguraatioihin, automaatioon ja sovellusten asetuksiin. Ohjelmoijat käyttävät YAML:ia sen selkeyden ja yksinkertaisuuden vuoksi, mikä tekee siitä ihanteellisen nopeasti muuttuviin ympäristöihin.

## How to: (Kuinka tehdä:)
C# koodiesimerkki, joka käsittelee YAML-dataa käyttäen YamlDotNet-kirjastoa. Asenna YamlDotNet nuget-paketista ennen koodin testaamista.

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var input = @"
            name: Mikko Mallikas
            age: 30
            occupation: Developer
        ";

        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        var person = deserializer.Deserialize<Person>(input);

        Console.WriteLine($"Nimi: {person.Name}, Ikä: {person.Age}, Ammatti: {person.Occupation}");
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Occupation { get; set; }
}
```

#### Tuloste:
```
Nimi: Mikko Mallikas, Ikä: 30, Ammatti: Developer
```

## Deep Dive (Syväsukellus)
YAML (YAML Ain't Markup Language) kehitettiin vuonna 2001, helpottamaan datan luettavuutta ja konfiguraatioita XML:n sijaan. YAML-formaattia vertaillaan usein JSON:iin; kumpikin on datan serialisointikieli, mutta YAML on usein helpommin luettavissa ihmisille. C# -ohjelmoijat voivat käyttää YamlDotNet-kirjastoa YAML-tiedostojen deserialisointiin ja serialisointiin. Kirjasto tukee sekä .NET Framework että .NET Core sovelluksia.

## See Also (Katso Myös)
- YamlDotNet GitHub: https://github.com/aaubry/YamlDotNet
- YAML virallinen sivusto: https://yaml.org
- Microsoftin C# dokumentaatio: https://docs.microsoft.com/en-us/dotnet/csharp/
