---
title:                "Att Arbeta med YAML"
date:                  2024-02-03T19:25:05.493844-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
YAML, som står för YAML Ain't Markup Language, är ett läsbart data-serialiseringsformat. Programmerare använder det ofta för konfigurationsfiler, inter-processmeddelanden och datalagring på grund av dess enkelhet och läsbarhet jämfört med andra dataformat som XML eller JSON.

## Hur man gör:
C# har inte inbyggt stöd för YAML, men du kan enkelt arbeta med YAML genom att använda tredjepartsbibliotek såsom *YamlDotNet*. Först behöver du installera YamlDotNet-paketet:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Läsa YAML:
Föreställ dig att du har en YAML-fil `config.yaml` med följande innehåll:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Du kan läsa och tolka denna YAML-fil i C# så här:
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
        en deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Justera namnkonventionen efter behov
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Namn: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**Exempelutdata:**
```
Namn: MyApp, Version: 1.0.0
```

### Skriva YAML:
För att skriva data till en YAML-fil använder du `Serializer`-klassen från YamlDotNet. Så här serialiserar du ett objekt tillbaka till YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Justera namnkonventionen efter behov
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Exempelutdata:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

Detta okomplicerade tillvägagångssätt demonstrerar hur du effektivt kan arbeta med YAML i dina C#-projekt, vilket gör det enkelt att läsa från och skriva till YAML-filer med hjälp av YamlDotNet-biblioteket.
