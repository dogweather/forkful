---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:43.456459-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : C# \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\
  \u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u0434\u043B\
  \u044F YAML, \u0430\u043B\u0435 \u0432\u0438 \u043B\u0435\u0433\u043A\u043E \u043C\
  \u043E\u0436\u0435\u0442\u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438\
  \ \u0437 YAML, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0447\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456 \u0431\u0456\
  \u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\u0456 \u044F\
  \u043A *YamlDotNet*.\u2026"
lastmod: '2024-03-13T22:44:49.320972-06:00'
model: gpt-4-0125-preview
summary: "C# \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u0434\
  \u043B\u044F YAML, \u0430\u043B\u0435 \u0432\u0438 \u043B\u0435\u0433\u043A\u043E\
  \ \u043C\u043E\u0436\u0435\u0442\u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\
  \u0442\u0438 \u0437 YAML, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0447\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A *YamlDotNet*."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

## Як це зробити:
C# не має вбудованої підтримки для YAML, але ви легко можете працювати з YAML, використовуючи сторонні бібліотеки, такі як *YamlDotNet*. Спочатку вам потрібно встановити пакет YamlDotNet:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Читання YAML:
Уявіть, що у вас є YAML-файл `config.yaml` з наступним вмістом:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Ви можете прочитати та розібрати цей YAML-файл у C# так:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Відповідно налаштуйте конвенцію іменування
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Назва: {config.appSettings.name}, Версія: {config.appSettings.version}");
    }
}
```
**Приклад виведення:**
```
Назва: MyApp, Версія: 1.0.0
```

### Запис YAML:
Щоб записати дані в YAML-файл, використовуйте клас `Serializer` з YamlDotNet. Ось як ви можете серіалізувати об'єкт назад у YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Відповідно налаштуйте конвенцію іменування
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Приклад виведення:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

Цей простий підхід демонструє, як ефективно працювати з YAML у ваших проектах на C#, роблячи читання з файлів YAML та запис у них простими завдяки використанню бібліотеки YamlDotNet.
