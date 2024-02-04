---
title:                "JSONを活用する"
date:                  2024-02-03T19:22:18.326507-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
