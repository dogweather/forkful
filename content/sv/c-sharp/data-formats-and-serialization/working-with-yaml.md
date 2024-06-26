---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:05.493844-07:00
description: "Hur man g\xF6r: C# har inte inbyggt st\xF6d f\xF6r YAML, men du kan\
  \ enkelt arbeta med YAML genom att anv\xE4nda tredjepartsbibliotek s\xE5som *YamlDotNet*.\
  \ F\xF6rst beh\xF6ver\u2026"
lastmod: '2024-03-13T22:44:37.933771-06:00'
model: gpt-4-0125-preview
summary: "C# har inte inbyggt st\xF6d f\xF6r YAML, men du kan enkelt arbeta med YAML\
  \ genom att anv\xE4nda tredjepartsbibliotek s\xE5som *YamlDotNet*."
title: Att Arbeta med YAML
weight: 41
---

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
