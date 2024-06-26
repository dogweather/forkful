---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:03.956789-07:00
description: "Comment faire : C# n'a pas de support int\xE9gr\xE9 pour YAML, mais\
  \ vous pouvez facilement travailler avec YAML en utilisant des biblioth\xE8ques\
  \ tierces telles\u2026"
lastmod: '2024-03-13T22:44:57.809895-06:00'
model: gpt-4-0125-preview
summary: "C# n'a pas de support int\xE9gr\xE9 pour YAML, mais vous pouvez facilement\
  \ travailler avec YAML en utilisant des biblioth\xE8ques tierces telles que *YamlDotNet*."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
C# n'a pas de support intégré pour YAML, mais vous pouvez facilement travailler avec YAML en utilisant des bibliothèques tierces telles que *YamlDotNet*. Tout d'abord, vous devez installer le package YamlDotNet :

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Lire du YAML :
Imaginez que vous ayez un fichier YAML `config.yaml` avec le contenu suivant :
```yaml
appSettings:
  nom: MonAppli
  version: 1.0.0
```

Vous pouvez lire et analyser ce fichier YAML en C# comme ceci :
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
    public string nom { get; set; }
    public string version { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Ajustez la convention de nommage en conséquence
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Nom: {config.appSettings.nom}, Version: {config.appSettings.version}");
    }
}
```
**Exemple de sortie :**
```
Nom: MonAppli, Version: 1.0.0
```

### Écrire dans un fichier YAML :
Pour écrire des données dans un fichier YAML, utilisez la classe `Serializer` de YamlDotNet. Voici comment sérialiser un objet en retour vers YAML :

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
                nom = "MonAppli",
                version = "2.0.0"
            }
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Ajustez la convention de nommage en conséquence
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Exemple de sortie :**
```yaml
appSettings:
  nom: MonAppli
  version: 2.0.0
```

Cette approche directe démontre comment travailler efficacement avec YAML dans vos projets C#, en facilitant la lecture et l'écriture de fichiers YAML à l'aide de la bibliothèque YamlDotNet.
