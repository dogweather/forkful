---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:08.064625-07:00
description: "\u65B9\u6CD5 C# \u306B\u306F YAML \u306E\u30D3\u30EB\u30C8\u30A4\u30F3\
  \u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001*YamlDotNet*\
  \ \u306A\u3069\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u7C21\u5358\u306B YAML\
  \ \u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u307E\u305A\
  \u3001YamlDotNet \u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\
  \u30EB\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.024184-06:00'
model: gpt-4-0125-preview
summary: ''
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法
C# には YAML のビルトインサポートはありませんが、*YamlDotNet* などのサードパーティライブラリを使用することで簡単に YAML を扱うことができます。まず、YamlDotNet パッケージをインストールする必要があります：

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### YAMLの読み込み：
以下の内容の YAML ファイル `config.yaml` を持っていると想像してください：
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

この YAML ファイルを C# で読み込んで解析するには、次のようにします：
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // 名前付け規則を適宜修正
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**サンプル出力：**
```
Name: MyApp, Version: 1.0.0
```

### YAMLへの書き込み：
YAMLファイルにデータを書き込むには、YamlDotNetの`Serializer`クラスを使用します。ここでは、オブジェクトをYAMLに戻してシリアライズする方法を示します：

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // 名前付け規則を適宜修正
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**サンプル出力：**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

この簡単なアプローチは、YamlDotNetライブラリを使用してC#プロジェクトでYAMLを効果的に扱う方法を実演しており、YAMLファイルの読み書きが簡単にできます。
