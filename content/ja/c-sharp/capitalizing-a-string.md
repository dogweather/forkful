---
title:                "C#: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にすることについて、なぜ誰かがそれに取り組みたがるのかを説明します。

## 方法

"```C#
// 入力文字列
string input = "hello world";
// 文字列を大文字に変換
string output = input.ToUpper();
// 出力
Console.WriteLine(output); // HELLO WORLD
```"

## ディープダイブ

文字列を大文字にする機能は、プログラムで非常に便利です。例えば、ユーザーの入力を確実に大文字に変換することで、入力ミスを防ぐことができます。また、大文字と小文字を区別しない検索やソートを行う際にも、大文字に統一することで正確な結果を得ることができます。

## 参考

[.NET フレームワーク ドキュメント - string.ToUpper メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.toupper?view=net-5.0)

[C# リファレンス - 文字列操作](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/strings/)