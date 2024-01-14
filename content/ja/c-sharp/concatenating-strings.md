---
title:    "C#: 文字列の連結"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することの重要性について、今回のブログポストではご紹介します。プログラミングにおいて文字列を連結する必要があるケースや、そのメリットについて知ることができるでしょう。

## 方法

文字列を連結するには、C#の簡単なメソッドを使用することができます。下記のコードブロックを参考にして、実際にコードを書いてみましょう。

```C#
string firstString = "Hello";
string secondString = "world";
string result = firstString + secondString; // Hello world
```

上記の例では、まず2つの文字列を定義しています。そして、連結のために"+"演算子を使用し、新しい変数に代入しています。これで、2つの文字列が連結されて出力されることが確認できます。

## 深堀

文字列の連結にはさまざまな方法があります。 "+"演算子以外にも、String.Join()メソッドやStringBuilderクラスを使用することもできます。また、文字列の連結にはパフォーマンスの問題もありますので、適切な方法を選択することが重要です。

## 併せて参照してください

- [C# String Concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/string-concatenation) (C＃の文字列連結についての公式ドキュメント)
- [C# String Methods](https://www.geeksforgeeks.org/c-sharp-string-methods/) (C#で利用できる便利な文字列メソッドについての解説)