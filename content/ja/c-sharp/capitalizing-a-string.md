---
title:                "「文字列を大文字化する」"
html_title:           "C#: 「文字列を大文字化する」"
simple_title:         "「文字列を大文字化する」"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の大文字変換を行う理由を楽しく説明する。多くのプログラマーにとって、文字列の大文字変換は基本的なタスクであり、これを行う方法を知っておくと便利です。

## 使い方
文字列の大文字変換は、C＃のStringクラスのメソッドを使用して行うことができます。例えば、次のように記述します。

```C#
string str = "こんにちは、世界！";
str = str.ToUpper();
Console.WriteLine(str);
```

上記のコードでは、元の文字列が大文字に変換され、"こんにちは、世界！" は "こんにちは、世界！" に変わります。また、特定の文字列を大文字に変換するには、 `ToUpper()` メソッドの代わりに `ToUpperInvariant()` メソッドを使用することもできます。これは、文化や言語に依存せず、常に同じ結果を返します。

## ディープダイブ
文字列を大文字に変換するメソッドは、プログラミング言語によって異なる場合があります。C＃では、文字列の一部を大文字に変換する `ToUpper()` メソッドが使用されますが、他の言語では `Uppercase()` メソッドや `Upper()` メソッドが使用される場合があります。さらに、文字列の大文字変換には、Unicodeに基づく変換や独自のアルゴリズムに基づく変換など、さまざまな方法があります。

## 参考リンク
- C＃のStringクラス: https://docs.microsoft.com/ja-jp/dotnet/api/system.string?view=net-5.0
- 全角・半角文字の変換について: https://qiita.com/gabapple/items/9ec753b47e11e8a58876
- Unicodeのオフィシャルサイト: https://www.unicode.org/