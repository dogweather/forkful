---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ?(What & Why?)

文字列の結合は、二つ以上の文字列を一つにつなげる処理です。プログラマは、情報表示やデータ操作に際して便利なために実行します。

## どうやって？(How to?)

以下にC#での文字列結合の基本的な方法を示します。

```C#
string firstName = "Naoto";
string lastName = "Tanaka";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```

出力結果は以下の通りです。

```C#
Naoto Tanaka
```

ここでは、二つの文字列をプラス記号(+)で結合し、新しい文字列を生成しています。

## より詳しく(Detail)

* 歴史的背景: 昔ながらの方法であるプラス(+)演算子を使用する方法は、わかりやすいですが、大量の文字列を結合する場合にはパフォーマンスが低下する可能性があります。
* 代替案: StringBuilderを利用すると、より効率的に大量の文字列を結合することができます。

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Naoto");
sb.Append(" ");
sb.Append("Tanaka");
string fullName = sb.ToString();
Console.WriteLine(fullName);
```

* 実装の詳細: StringBuilderは、内部的には配列のリサイズを避けるために、一連の文字列ブロックを保持します。最後にToStringメソッドで一つの文字列に変換します。

## 参照元(Reference)

* [C# 文字列操作(Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/strings/)
* [C#: 文字列の結合、分割(StackOverflow)](https://ja.stackoverflow.com/questions/579/%E6%96%87%E5%AD%97%E5%88%97%E3%81%AE%E7%B5%90%E5%90%88-%E5%88%86%E5%89%B2)