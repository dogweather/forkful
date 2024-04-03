---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:01.323280-07:00
description: "Jak to zrobi\u0107: C# nie ma wbudowanego wsparcia dla YAML, ale mo\u017C\
  na \u0142atwo pracowa\u0107 z YAML, u\u017Cywaj\u0105c bibliotek stron trzecich,\
  \ takich jak *YamlDotNet*.\u2026"
lastmod: '2024-03-13T22:44:35.429246-06:00'
model: gpt-4-0125-preview
summary: "C# nie ma wbudowanego wsparcia dla YAML, ale mo\u017Cna \u0142atwo pracowa\u0107\
  \ z YAML, u\u017Cywaj\u0105c bibliotek stron trzecich, takich jak *YamlDotNet*."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
C# nie ma wbudowanego wsparcia dla YAML, ale można łatwo pracować z YAML, używając bibliotek stron trzecich, takich jak *YamlDotNet*. Najpierw musisz zainstalować pakiet YamlDotNet:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Odczytywanie YAML:
Wyobraź sobie, że masz plik YAML `config.yaml` z następującą zawartością:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Możesz odczytać i zanalizować ten plik YAML w C# w ten sposób:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Dostosuj konwencję nazewnictwa odpowiednio
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**Przykładowe wyjście:**
```
Name: MyApp, Version: 1.0.0
```

### Zapisywanie YAML:
Aby zapisać dane do pliku YAML, użyj klasy `Serializer` z YamlDotNet. Oto jak serializować obiekt z powrotem do YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Dostosuj konwencję nazewnictwa odpowiednio
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Przykładowe wyjście:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

To proste podejście pokazuje, jak efektywnie pracować z YAML w projektach C#, umożliwiając łatwe odczytywanie z i zapisywanie do plików YAML za pomocą biblioteki YamlDotNet.
