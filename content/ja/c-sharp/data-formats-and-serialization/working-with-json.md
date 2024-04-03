---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:18.326507-07:00
description: "JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:42.152979-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript Object Notation\uFF09\u3092\u6271\u3046\u3068\u306F\
  \u3001JSON\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3001\u751F\u6210\u3001\u304A\u3088\
  \u3073\u30AF\u30A8\u30EA\u3092\u884C\u3046\u3053\u3068\u3092\u610F\u5473\u3057\u3001\
  \u73FE\u4EE3\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\
  \u91CD\u8981\u306A\u30B9\u30AD\u30EB\u3068\u306A\u308A\u307E\u3059\u3002\u3053\u306E\
  \u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306F\u3001\u305D\
  \u306E\u8AAD\u307F\u3084\u3059\u3055\u3068\u8A00\u8A9E\u306E\u72EC\u7ACB\u6027\u306E\
  \u305F\u3081\u3001Web\u30B5\u30FC\u30D3\u30B9\u3084API\u3067\u5E83\u304F\u4F7F\u7528\
  \u3055\u308C\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30CD\u30C3\u30C8\
  \u30EF\u30FC\u30AF\u5316\u3055\u308C\u305F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u3092\u958B\u767A\u3059\u308B\u304B\u3001Web\u30D9\u30FC\u30B9\u306E\u30C7\
  \u30FC\u30BF\u3068\u5BFE\u8A71\u3059\u308BC#\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306B\u3068\u3063\u3066\u5FC5\u9808\u3067\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## はじめに: 何となぜ?

JSON（JavaScript Object Notation）を扱うとは、JSONデータの解析、生成、およびクエリを行うことを意味し、現代のプログラミングにおいて重要なスキルとなります。このデータ交換フォーマットは、その読みやすさと言語の独立性のため、WebサービスやAPIで広く使用されています。これは、ネットワーク化されたアプリケーションを開発するか、Webベースのデータと対話するC#プログラマーにとって必須です。

## 方法:

### JSON文字列をオブジェクトに解析する

C#は`System.Text.Json`名前空間を提供し、効率的なJSON処理を実現します。JSON文字列をC#オブジェクトに解析するには、JSON構造に一致するクラスを定義し、`JsonSerializer.Deserialize`メソッドを使用します。

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // 出力: Name: John, Age: 30
    }
}
```

### オブジェクトからJSONへ変換する

C#オブジェクトをJSON文字列に戻すためには、`JsonSerializer.Serialize`メソッドを使用します。

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // 出力: {"Name":"Jane","Age":25}
    }
}
```

### Newtonsoft.Jsonの使用

`Newtonsoft.Json`（またはJson.NET）は、JSONのシリアライズおよびデシリアライズに関してより柔軟性とオプションを提供する、人気のあるサードパーティ製ライブラリです。

Json.NETを使うには、最初にNuGet経由で`Newtonsoft.Json`パッケージをインストールする必要があります。その後、次のようにJSON文字列のデシリアライズができます：

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // 出力: Name: Mike, Age: 22
    }
}
```

オブジェクトからJSONを生成する場合、Json.NETを使用：

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // 出力: {"Name":"Ella","Age":28}
    }
}
```

これらのスニペットは、C#でのJSONの取り扱いについてさっと学ぶためのものであり、組み込みの`System.Text.Json`の機能と`Newtonsoft.Json`の広範な機能の両方を示しています。
