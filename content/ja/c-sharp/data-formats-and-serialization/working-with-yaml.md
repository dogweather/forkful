---
title:                "YAML を操作する"
aliases:
- /ja/c-sharp/working-with-yaml/
date:                  2024-02-03T19:25:08.064625-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
