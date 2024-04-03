---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:00.344004-07:00
description: "\u5982\u4F55\u64CD\u4F5C: C# \u6CA1\u6709\u5185\u7F6E\u5BF9 YAML \u7684\
  \u652F\u6301\uFF0C\u4F46\u4F60\u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528\u7B2C\u4E09\u65B9\
  \u5E93\u5982 *YamlDotNet* \u6765\u8F7B\u677E\u5730\u5904\u7406 YAML\u3002\u9996\u5148\
  \uFF0C\u4F60\u9700\u8981\u5B89\u88C5 YamlDotNet \u5305\uFF1A."
lastmod: '2024-03-13T22:44:47.790531-06:00'
model: gpt-4-0125-preview
summary: "C# \u6CA1\u6709\u5185\u7F6E\u5BF9 YAML \u7684\u652F\u6301\uFF0C\u4F46\u4F60\
  \u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u5982 *YamlDotNet*\
  \ \u6765\u8F7B\u677E\u5730\u5904\u7406 YAML\u3002\u9996\u5148\uFF0C\u4F60\u9700\u8981\
  \u5B89\u88C5 YamlDotNet \u5305\uFF1A."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
