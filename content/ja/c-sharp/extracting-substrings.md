---
title:                "「部分文字列の抽出」"
html_title:           "C#: 「部分文字列の抽出」"
simple_title:         "「部分文字列の抽出」"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
この記事では、C#でサブストリングを抽出する方法を解説します。サブストリングを抽出することで、文字列の一部を取り出して、必要な部分だけを処理することができます。

## 方法
サブストリングを抽出する方法は、以下のようになります。

```C#
// 元の文字列
string originalString = "こんにちは、私の名前は太郎です";

// サブストリングを抽出する
string subString = originalString.Substring(3, 5);

// 出力
Console.WriteLine(subString); // 出力結果: ちは、私
```

この例では、`Substring()`メソッドを使用して、元の文字列から3番目の文字から5文字分のサブストリングを抽出し、出力しています。カウントは0から始まることに注意してください。

また、特定の文字列を含むかどうかを判断する場合は、`Contains()`メソッドを使用することで、より簡単に実装することができます。

## ディープダイブ
サブストリングを抽出するには、`Substring()`メソッドの他にも、`Split()`メソッドや正規表現を使用する方法などがあります。また、文字列の先頭や末尾から抽出する場合は、`Trim()`メソッドを使用すると便利です。

## See Also
- [.NET Strings Cheat Sheet - Substrings in C#](https://cheatography.com/davechild/cheat-sheets/net-strings/)
- [C# String.Substring Examples](https://www.dotnetperls.com/substring)
- [Understanding String Manipulation in C#](https://www.c-sharpcorner.com/article/string-manipulation-in-c-sharp/)