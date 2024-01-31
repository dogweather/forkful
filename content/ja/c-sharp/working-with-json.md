---
title:                "JSONを扱う方法"
date:                  2024-01-19
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
JSONデータを扱うことは、アプリケーション間の軽量で読みやすいデータ交換に欠かせない。プログラマーはデータの保存、設定の管理、Web APIの利用にJSONを利用します。

## How to: (実践方法)
C#でJSONを扱うのには、`System.Text.Json`を使います。以下に基本的な使い方を例示します。

```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        var jsonString = "{\"name\":\"Yamada\",\"age\":30}";
        var person = JsonSerializer.Deserialize<Person>(jsonString);
        
        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}
```

出力:
```
Name: Yamada, Age: 30
```

オブジェクトをJSON文字列に変換する例です。

```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        var person = new Person
        {
            Name = "Suzuki",
            Age = 25
        };
        
        var jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}
```

出力:
```
{"name":"Suzuki","age":25}
```

## Deep Dive (深掘り)
JSON(JavaScript Object Notation)は2000年代初頭に登場しました。JavaScriptから派生したデータ表現ですが、言語に依存しないため、幅広いプログラミング言語で採用されています。`System.Text.Json`は高性能で、`Newtonsoft.Json`のような他のライブラリよりも.NET環境に統合されています。しかし、特定の機能では`Newtonsoft.Json`が依然として人気があります。

## See Also (関連情報)
- JSONの基本: https://www.json.org/json-ja.html
- `System.Text.Json`ドキュメント: https://docs.microsoft.com/ja-jp/dotnet/api/system.text.json?view=net-5.0
- `Newtonsoft.Json` (Json.NET): https://www.newtonsoft.com/json
