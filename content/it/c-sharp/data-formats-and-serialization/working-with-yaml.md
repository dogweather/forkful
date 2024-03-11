---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:54.433718-07:00
description: "YAML, acronimo di YAML Ain't Markup Language, \xE8 un formato di serializzazione\
  \ di dati leggibile dall'uomo. I programmatori lo usano spesso per file di\u2026"
lastmod: '2024-03-11T00:14:17.044038-06:00'
model: gpt-4-0125-preview
summary: "YAML, acronimo di YAML Ain't Markup Language, \xE8 un formato di serializzazione\
  \ di dati leggibile dall'uomo. I programmatori lo usano spesso per file di\u2026"
title: Lavorare con YAML
---

{{< edit_this_page >}}

## Cosa & Perché?
YAML, acronimo di YAML Ain't Markup Language, è un formato di serializzazione di dati leggibile dall'uomo. I programmatori lo usano spesso per file di configurazione, messaggistica tra processi e archiviazione dati a causa della sua semplicità e leggibilità rispetto ad altri formati di dati come XML o JSON.

## Come fare:
C# non dispone di supporto integrato per YAML, ma è possibile lavorare facilmente con YAML utilizzando librerie di terze parti come *YamlDotNet*. Prima di tutto, è necessario installare il pacchetto YamlDotNet:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Leggere YAML:
Immagina di avere un file YAML `config.yaml` con il seguente contenuto:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Puoi leggere e analizzare questo file YAML in C# così:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Regola la convenzione di denominazione di conseguenza
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Nome: {config.appSettings.name}, Versione: {config.appSettings.version}");
    }
}
```
**Esempio di Output:**
```
Nome: MyApp, Versione: 1.0.0
```

### Scrivere YAML:
Per scrivere dati in un file YAML, utilizza la classe `Serializer` di YamlDotNet. Ecco come serializzare un oggetto di nuovo in YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Regola la convenzione di denominazione di conseguenza
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Esempio di Output:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

Questo approccio diretto dimostra come lavorare in modo efficiente con YAML nei tuoi progetti C#, rendendo semplice la lettura e la scrittura di file YAML utilizzando la libreria YamlDotNet.
