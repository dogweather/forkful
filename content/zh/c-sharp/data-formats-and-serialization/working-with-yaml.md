---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:00.344004-07:00
description: "YAML\uFF0C\u4EE3\u8868\u201CYAML Ain't Markup Language\u201D\uFF08YAML\u4E0D\
  \u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\
  \u4F7F\u7528\u5B83\u6765\u5904\u7406\u914D\u7F6E\u6587\u4EF6\u3001\u8FDB\u7A0B\u95F4\
  \u6D88\u606F\u4F20\u9012\u548C\u6570\u636E\u5B58\u50A8\uFF0C\u56E0\u4E3A\u4E0EXML\u6216\
  JSON\u7B49\u5176\u4ED6\u6570\u636E\u683C\u5F0F\u76F8\u6BD4\uFF0C\u5B83\u7684\u7B80\
  \u5355\u6027\u548C\u53EF\u8BFB\u6027\u3002"
lastmod: '2024-02-25T18:49:45.358973-07:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u4EE3\u8868\u201CYAML Ain't Markup Language\u201D\uFF08YAML\u4E0D\
  \u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\
  \u4F7F\u7528\u5B83\u6765\u5904\u7406\u914D\u7F6E\u6587\u4EF6\u3001\u8FDB\u7A0B\u95F4\
  \u6D88\u606F\u4F20\u9012\u548C\u6570\u636E\u5B58\u50A8\uFF0C\u56E0\u4E3A\u4E0EXML\u6216\
  JSON\u7B49\u5176\u4ED6\u6570\u636E\u683C\u5F0F\u76F8\u6BD4\uFF0C\u5B83\u7684\u7B80\
  \u5355\u6027\u548C\u53EF\u8BFB\u6027\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
