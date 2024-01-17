---
title:                "stringsの結合"
html_title:           "C#: stringsの結合"
simple_title:         "stringsの結合"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何をして、なぜ？ 
文字列を連結するとは、単純に文字列をつなげることです。プログラマーがこれを行う理由は、様々な文字列データを一つにまとめることができるからです。

## 方法: 
文字列を連結するには、```C# ... ``` のコードブロック内でサンプルコードと出力を使用します。例えば、以下のように使用します。

```C#
string firstName = "John";
string lastName = "Doe";
Console.WriteLine($"My name is {firstName} {lastName}."); 
```

出力は以下のようになります。
```C#
My name is John Doe.
```

## ディープダイブ: 
文字列の連結は、古くから使われてきたプログラミングの基本的なテクニックです。他の方法として、文字列フォーマットや置換を利用することもできます。文字列の連結は、メモリの使用やパフォーマンスにも影響を与えるので注意が必要です。

## 参考: 
関連情報を以下のリンクから参照できます。
- [C#で文字列を連結する方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/how-to/concatenate-multiple-strings)
- [文字列をフォーマットする方法](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/formatting-types)
- [置換について学ぶ](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/how-to-do-a-substitution-in-strings)