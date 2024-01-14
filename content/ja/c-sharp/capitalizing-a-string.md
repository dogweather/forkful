---
title:    "C#: 文字列の大文字化"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

何かをプログラミングするとき、時々文の頭文字を大文字にする必要があります。今回は、C#で文字列を大文字にする方法について説明します。

## 方法

文の頭文字を大文字にするためには、 `ToUpper()` メソッドを使用します。以下のようにコードを書きます。

``` C#
string str = "hello world";
str = str.ToUpper();

Console.WriteLine(str);
```

このコードを実行すると、 `HELLO WORLD` という出力が得られます。 `ToUpper()` メソッドは、与えられた文字列の文字をすべて大文字に変換します。

## ディープダイブ

`ToUpper()` メソッドは、すべての言語で使えるわけではありません。特定の言語やカルチャーによって、大文字小文字のルールが異なります。そのため、必ずしも入力した文字列が意図した通りに変換されるとは限りません。また、日本語のように、大文字や小文字の概念自体が存在しない言語もあります。

さらに、C#では `ToLower()` メソッドも使用でき、文字列をすべて小文字に変換することもできます。 `ToUpper()` と `ToLower()` を組み合わせて使うことで、より複雑な文字列操作が可能になります。

## 参考リンク

- [C# 公式ドキュメント - ToUpper メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.toupper)
- [C# 公式ドキュメント - ToLower メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.tolower)
- [C# String の大文字・小文字関連のメソッドまとめ](https://www.sejuku.net/blog/57559)
- [C# 文字列演算子](https://csharp.keicode.com/lang/string.php)