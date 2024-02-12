---
title:                "Arbeider med YAML"
aliases:
- no/c-sharp/working-with-yaml.md
date:                  2024-02-03T19:24:53.780222-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML, som står for YAML Ain't Markup Language, er et menneskelesbart data serialiseringsformat. Programmerere bruker det ofte for konfigurasjonsfiler, mellomprosessmeldinger, og datalagring på grunn av dets enkelhet og lesbarhet sammenlignet med andre dataformater som XML eller JSON.

## Hvordan:
C# har ikke innebygd støtte for YAML, men du kan enkelt arbeide med YAML ved å bruke tredjepartsbiblioteker som *YamlDotNet*. Først må du installere YamlDotNet-pakken:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Lesing av YAML:
Forestille deg at du har en YAML-fil `config.yaml` med følgende innhold:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Du kan lese og analysere denne YAML-filen i C# slik:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Juster navnekonvensjonen deretter
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**Eksempel på utdata:**
```
Name: MyApp, Version: 1.0.0
```

### Skriving av YAML:
For å skrive data til en YAML-fil, bruk `Serializer`-klassen fra YamlDotNet. Her er hvordan du serialiserer et objekt tilbake til YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Juster navnekonvensjonen deretter
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Eksempel på utdata:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

Denne enkle tilnærmingen demonstrerer hvordan du kan arbeide effektivt med YAML i dine C#-prosjekter, og gjør det enkelt å lese fra og skrive til YAML-filer ved å bruke YamlDotNet-biblioteket.
