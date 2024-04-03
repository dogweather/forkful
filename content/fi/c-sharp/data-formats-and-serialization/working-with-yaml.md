---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:01.705830-07:00
description: "Kuinka: C# ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua tukea YAML:lle,\
  \ mutta voit helposti ty\xF6skennell\xE4 YAML:n kanssa k\xE4ytt\xE4m\xE4ll\xE4 kolmannen\
  \ osapuolen kirjastoja, kuten\u2026"
lastmod: '2024-03-13T22:44:56.594429-06:00'
model: gpt-4-0125-preview
summary: "C# ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua tukea YAML:lle, mutta voit helposti\
  \ ty\xF6skennell\xE4 YAML:n kanssa k\xE4ytt\xE4m\xE4ll\xE4 kolmannen osapuolen kirjastoja,\
  \ kuten *YamlDotNet*."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
C# ei sisällä sisäänrakennettua tukea YAML:lle, mutta voit helposti työskennellä YAML:n kanssa käyttämällä kolmannen osapuolen kirjastoja, kuten *YamlDotNet*. Ensin sinun täytyy asentaa YamlDotNet-paketti:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### YAML:n lukeminen:
Kuvittele, että sinulla on YAML-tiedosto `config.yaml` seuraavalla sisällöllä:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Voit lukea ja jäsentää tämän YAML-tiedoston C#:ssa näin:
```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class AppConfig
{
    public AppSettings appSettings { get; set; }
}

public class AppSettings
{
    public string name { get; set; }
    public string version { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Sovita nimeämiskonventio tarpeen mukaan
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Nimi: {config.appSettings.name}, Versio: {config.appSettings.version}");
    }
}
```
**Esimerkkituloste:**
```
Nimi: MyApp, Versio: 1.0.0
```

### YAML:n kirjoittaminen:
Jotta voit kirjoittaa dataa YAML-tiedostoon, käytä `Serializer`-luokkaa YamlDotNetistä. Tässä on, miten voit sarjallistaa olion takaisin YAML:ksi:

```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program
{
    static void Main(string[] args)
    {
        var config = new AppConfig
        {
            appSettings = new AppSettings
            {
                name = "MyApp",
                version = "2.0.0"
            }
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Sovita nimeämiskonventio tarpeen mukaan
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Esimerkkituloste:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

Tämä suoraviivainen lähestymistapa osoittaa, miten voit tehokkaasti työskennellä YAML:n kanssa C# projekteissasi. Se tekee YAML-tiedostojen lukemisesta ja kirjoittamisesta yksinkertaista käyttäen YamlDotNet-kirjastoa.
