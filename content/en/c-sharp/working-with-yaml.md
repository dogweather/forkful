---
title:                "Working with YAML"
aliases:
- en/c-sharp/working-with-yaml.md
date:                  2024-02-03T19:03:15.985684-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, which stands for YAML Ain't Markup Language, is a human-readable data serialization format. Programmers often use it for configuration files, inter-process messaging, and data storage because of its simplicity and readability compared to other data formats like XML or JSON.

## How to:
C# doesn't have built-in support for YAML, but you can easily work with YAML by using third-party libraries such as *YamlDotNet*. First, you need to install the YamlDotNet package:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Reading YAML:
Imagine you have a YAML file `config.yaml` with the following content:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

You can read and parse this YAML file in C# like this:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Adjust the naming convention accordingly
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**Sample Output:**
```
Name: MyApp, Version: 1.0.0
```

### Writing YAML:
To write data to a YAML file, use the `Serializer` class from YamlDotNet. Here is how you serialize an object back to YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Adjust the naming convention accordingly
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Sample Output:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

This straightforward approach demonstrates how to efficiently work with YAML in your C# projects, making it simple to read from and write to YAML files using the YamlDotNet library.
