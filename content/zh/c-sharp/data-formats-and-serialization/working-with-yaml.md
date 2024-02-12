---
title:                "使用YAML工作"
aliases:
- /zh/c-sharp/working-with-yaml/
date:                  2024-02-03T19:25:00.344004-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?
YAML，代表“YAML Ain't Markup Language”（YAML不是标记语言），是一种人类可读的数据序列化格式。程序员经常使用它来处理配置文件、进程间消息传递和数据存储，因为与XML或JSON等其他数据格式相比，它的简单性和可读性。

## 如何操作:
C# 没有内置对 YAML 的支持，但你可以通过使用第三方库如 *YamlDotNet* 来轻松地处理 YAML。首先，你需要安装 YamlDotNet 包：

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### 读取 YAML:
假设你有一个包含以下内容的 YAML 文件 `config.yaml`:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

你可以像这样在 C# 中读取并解析这个 YAML 文件:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // 根据需要调整命名约定
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**样例输出:**
```
Name: MyApp, Version: 1.0.0
```

### 写入 YAML:
要将数据写入 YAML 文件，请使用 YamlDotNet 的 `Serializer` 类。以下是如何将对象序列化回 YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // 根据需要调整命名约定
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**样例输出:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

这种直接的方法展示了如何在你的 C# 项目中高效地处理 YAML，使用 YamlDotNet 库可以简单地从 YAML 文件读取和写入数据。
