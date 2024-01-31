---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

YAMLは設定ファイルやデータ交換に使うフォーマット。C#でYAMLを扱うと、設定が簡単でわかりやすくなります。

## How to: (やり方)

```csharp
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var yml = @"
name: John Doe
age: 30
isProgrammer: true
";
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        var person = deserializer.Deserialize<Person>(yml);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}, IsProgrammer: {person.IsProgrammer}");
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public bool IsProgrammer { get; set; }
}
```
出力:
```
Name: John Doe, Age: 30, IsProgrammer: True
```

## Deep Dive (詳細な解説)

YAMLが使われ始めたのは2001年。JSONやXMLと比較して人間が読み書きしやすい。C#ではYamlDotNetのようなライブラリで処理。オブジェクトへのシリアライズとデシリアライズがメイン。

## See Also (参考情報)

- YamlDotNet GitHub Repository: [https://github.com/aaubry/YamlDotNet](https://github.com/aaubry/YamlDotNet)
- YAML公式サイト: [https://yaml.org](https://yaml.org)
- .NETのシリアライズ: [https://docs.microsoft.com/dotnet/standard/serialization/](https://docs.microsoft.com/dotnet/standard/serialization/)
