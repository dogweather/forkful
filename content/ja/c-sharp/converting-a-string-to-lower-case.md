---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 小文字への変換: それは何? なぜk必要なのか?

小文字への変換は、大文字が含まれる文字列をすべて同じ小文字に変えるプログラム操作です。一貫性のため、また、ユーザーインプットとプログラムコードの比較を簡易化するために行います。

# 実施方法

以下にC#を用いた実際の例を示します。 

```C#
string myString = "Hello, World!";
string lowerCaseString = myString.ToLower();
Console.WriteLine(lowerCaseString);
```
上記コードの出力は以下の通りです。

```C#
hello, world!
```

# より深く掘り下げる

**歴史的な文脈**: .NETが最初にリリースされた頃から `ToLower()` メソッドは存在しています。それは、開発者が大文字小文字を問わず使用できるようにし、比較を簡単にするための基本的な方法です。

**代替方法**: もし一部のロケールに対応する必要があるなら `ToLowerInvariant()` を使用することもできます。

```C#
string someString = "Ola, Mundo!";
Console.WriteLine(someString.ToLowerInvariant());
```
**実装詳細**: `ToLower()` メソッドは、対象となる文字列の各文字を対応する小文字に置き換えます。文字列がNullの場合、そのままNullが返ります。なお、このメソッドは文字列の元の値を変更しません。新しい文字列が生成され、その値が返されます。

# 関連情報

- 文字列を大文字に変換するには: [ToUpper Method](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.toupper)
- 文字列比較についての更なる情報: [String Comparison in C#](https://docs.microsoft.com/ja-jp/dotnet/csharp/how-to/compare-strings)
- 文字列操作に関する一般的な情報: [String Operations in C#](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/strings/)