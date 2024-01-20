---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що і чому?
YAML - це мова серіалізації даних, яка важлива для конфігурацій та обміну даними між мовами. Програмісти використовують її, бо вона легка для читання людиною і зручна для конфігурації проектів.

## Як це зробити:
Для роботи з YAML у C# використовуємо бібліотеку YamlDotNet. Спершу встановлюємо її через NuGet:

```C#
// Підключення YamlDotNet через NuGet Package Manager console
Install-Package YamlDotNet
```
Тепер читаємо YAML-файл і десеріалізуємо його:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;
using System.IO;

class Program
{
    static void Main()
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        
        var config = deserializer.Deserialize<Config>(yaml);
        
        Console.WriteLine($"Title: {config.Title}, Version: {config.Version}");
    }
}

public class Config
{
    public string Title { get; set; }
    public int Version { get; set; }
}
```
Якщо маємо `config.yaml` з вмістом:
```yaml
title: Example Project
version: 1
```
Виведеться:
```
Title: Example Project, Version: 1
```

## Поглиблення:
YAML, або "YAML Ain't Markup Language", з'явився у 2001 році. Він став альтернативою XML і JSON, пропонуючи зручніше для людини форматування. У C#, крім YamlDotNet, мало альтернатив для роботи з YAML. Важливо знати, що YAML чутливий до відступів та може представляти дані у вигляді списків, словників та скалярних значень.

## Дивись також:
- Офіційний сайт YAML: https://yaml.org
- Репозиторій YamlDotNet на GitHub: https://github.com/aaubry/YamlDotNet
- YAML специфікація: https://yaml.org/spec/1.2/spec.html