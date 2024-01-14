---
title:                "C#: 文字列の抽出"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

サブストリングの抽出を行う理由はさまざまです。例えば、与えられた文字列から特定の部分のみを抜き出す必要がある場合や、文字列をより小さな単位で処理する必要がある場合に、サブストリングの抽出が役立ちます。C#では、文字列操作を行う際にサブストリングを抽出する方法が提供されています。

## 技法

```C#
// 文字列を定義する
string str = "今日はいい天気です。";

// 文字列の一部分を抜き出す
string subStr = str.Substring(3,2);

// 結果を出力する
Console.WriteLine(subStr);

// 出力結果: いい
```

上記の例では、`Substring()`メソッドを使用して与えられた文字列から指定された位置の部分文字列を抜き出しています。メソッドの第一引数は抜き出す部分文字列の開始位置、第二引数は抜き出す文字数を指定します。このようにして、文字列の任意の部分を抜き出すことができます。

## 深堀り

サブストリングを抽出する際には、正確な位置や文字数を指定することが重要です。また、文字列の長さをチェックする必要もあります。C#では、文字列のメソッドやプロパティを使用して、これらの情報に簡単にアクセスすることができます。さらに、正規表現を使用することで、より柔軟な文字列の操作が可能になります。サブストリングの抽出について詳しく学びたい方は、これらのトピックを深く掘り下げることをお勧めします。

## 参考

- [Microsoft Docs: String.Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netcore-3.1)
- [C# Tutorial: String Methods and Properties](https://www.tutorialspoint.com/csharp/csharp_string_methods.htm)
- [C# Tutorial: Regular Expressions](https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm)

## 参考リンク