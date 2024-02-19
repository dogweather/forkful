---
aliases:
- /ja/c-sharp/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:08.064625-07:00
description: "YAML\u306F\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\u3001\
  \u4EBA\u9593\u304C\u8AAD\u3081\u308B\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\u3059\u3002\u305D\u306E\u30B7\u30F3\
  \u30D7\u30EB\u3055\u3068\u8AAD\u307F\u3084\u3059\u3055\u304B\u3089\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30D7\u30ED\
  \u30BB\u30B9\u9593\u30E1\u30C3\u30BB\u30FC\u30B8\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\
  \u30B9\u30C8\u30EC\u30FC\u30B8\u306BXML\u3084JSON\u306A\u3069\u306E\u4ED6\u306E\u30C7\
  \u30FC\u30BF\u5F62\u5F0F\u3068\u6BD4\u3079\u3066YAML\u3092\u3088\u304F\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.936204
model: gpt-4-0125-preview
summary: "YAML\u306F\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\u3001\
  \u4EBA\u9593\u304C\u8AAD\u3081\u308B\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\u3059\u3002\u305D\u306E\u30B7\u30F3\
  \u30D7\u30EB\u3055\u3068\u8AAD\u307F\u3084\u3059\u3055\u304B\u3089\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30D7\u30ED\
  \u30BB\u30B9\u9593\u30E1\u30C3\u30BB\u30FC\u30B8\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\
  \u30B9\u30C8\u30EC\u30FC\u30B8\u306BXML\u3084JSON\u306A\u3069\u306E\u4ED6\u306E\u30C7\
  \u30FC\u30BF\u5F62\u5F0F\u3068\u6BD4\u3079\u3066YAML\u3092\u3088\u304F\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
YAMLは「YAML Ain't Markup Language」の略で、人間が読めるデータシリアライゼーション形式です。そのシンプルさと読みやすさから、プログラマーは設定ファイル、プロセス間メッセージング、データストレージにXMLやJSONなどの他のデータ形式と比べてYAMLをよく使用します。

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
