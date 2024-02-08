---
title:                "Робота з YAML"
aliases:
- uk/c-sharp/working-with-yaml.md
date:                  2024-02-03T19:25:43.456459-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
YAML, що означає "YAML Ain't Markup Language" (YAML - це не мова розмітки), є форматом серіалізації даних, зрозумілим для людини. Програмісти часто використовують його для файлів конфігурації, міжпроцесного обміну повідомленнями та зберігання даних через його простоту та зрозумілість у порівнянні з іншими форматами даних, такими як XML або JSON.

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
