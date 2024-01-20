---
title:                "「yamlの操作方法」"
html_title:           "C#: 「yamlの操作方法」"
simple_title:         "「yamlの操作方法」"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## これはなに？

YAMLとは、プログラマーがデータをよりクリーンに、より読みやすく表現するために使用するフォーマットです。プログラマーはYAMLを使うことで、コードを簡単に管理できるようになり、可読性を向上させることができます。

## 使い方：

```C#
var person = new Person
{
    Name: "John",
    Age: 27
};

string yaml = person.ToYaml();
Console.WriteLine(yaml)

// Output:
//
// Name: John
// Age: 27
```

## ディープダイブ：

YAMLが作られたのは、2001年にRubyの開発者であるIngy döt Netによってです。YAMLはXMLやJSONなどの代替手段として使用されることがありますが、YAMLはシンプルで読みやすい表現方法を提供します。

### 実装の詳細：

YAMLは、データの構造をインデントによって表現します。また、キーと値をコロンで分けて表記します。このような特徴により、YAMLは人間にとってもプログラムにとっても扱いやすくなります。

## 関連リンク：

- [YAML公式サイト](https://yaml.org/)
- [YAMLの仕様書](https://yaml.org/spec/1.2/spec.html)