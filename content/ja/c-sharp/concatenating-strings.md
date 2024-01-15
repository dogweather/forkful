---
title:                "文字列の結合"
html_title:           "C#: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列を連結することには、テキストやデータの処理において非常に便利な方法があります。例えば、ユーザーの名前とメールアドレスを結合してメールの本文を作成する際、文字列連結を使うことで簡単に実現できます。

## 使い方
次の例では、```C#```のコードブロックを使用して、複数の文字列を連結する方法を示します。

```C#
string firstName = "John";
string lastName = "Smith";
string fullName = firstName + " " + lastName;

Console.WriteLine("Full Name: " + fullName);
```

上記のコードを実行すると、コンソールには次のように出力されます。

```
Full Name: John Smith
```

## ディープダイブ
文字列を連結する方法は複数あり、使用する方法によってパフォーマンスに違いがあります。ここでは、```string.Concat()```メソッドを使用する方法を紹介します。

```C#
string firstName = "John";
string lastName = "Smith";
string fullName = string.Concat(firstName, " ", lastName);

Console.WriteLine("Full Name: " + fullName);
```

上記の例では、```string.Concat()```メソッドを使用して複数の文字列を連結しました。このメソッドは、与えられた文字列を1つの文字列に結合する際に高速に動作するため、大きな文字列を連結する場合には有用です。

## もっと詳しく
文字列の連結方法について詳しく知りたい場合は、[公式ドキュメント](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)を参照してください。

## 同様の記事
- [C#入門ガイド](https://www.techacademy.jp/magazine/10117)
- [C#で文字列を扱う方法](https://www.atmarkit.co.jp/ait/articles/1910/18/news027.html)