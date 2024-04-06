---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:18.326507-07:00
description: "\u65B9\u6CD5: C#\u306F`System.Text.Json`\u540D\u524D\u7A7A\u9593\u3092\
  \u63D0\u4F9B\u3057\u3001\u52B9\u7387\u7684\u306AJSON\u51E6\u7406\u3092\u5B9F\u73FE\
  \u3057\u307E\u3059\u3002JSON\u6587\u5B57\u5217\u3092C#\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u306B\u89E3\u6790\u3059\u308B\u306B\u306F\u3001JSON\u69CB\u9020\u306B\u4E00\
  \u81F4\u3059\u308B\u30AF\u30E9\u30B9\u3092\u5B9A\u7FA9\u3057\u3001`JsonSerializer.Deserialize`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.152979-06:00'
model: gpt-4-0125-preview
summary: "C#\u306F`System.Text.Json`\u540D\u524D\u7A7A\u9593\u3092\u63D0\u4F9B\u3057\
  \u3001\u52B9\u7387\u7684\u306AJSON\u51E6\u7406\u3092\u5B9F\u73FE\u3057\u307E\u3059\
  \u3002JSON\u6587\u5B57\u5217\u3092C#\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u89E3\
  \u6790\u3059\u308B\u306B\u306F\u3001JSON\u69CB\u9020\u306B\u4E00\u81F4\u3059\u308B\
  \u30AF\u30E9\u30B9\u3092\u5B9A\u7FA9\u3057\u3001`JsonSerializer.Deserialize`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u307E\u3059."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
