---
title:                "文字列を小文字に変換する"
html_title:           "C#: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何をして、なぜそれをするのか？

文字列を小文字に変換することは、プログラマーにとって非常によく使われる技術です。これは、文字列の大文字や小文字を区別する場合や、文字列の比較を行う場合に役立ちます。また、文字列を小文字化することで、入力データの統一性を保つことができます。

## 方法：

文字列を小文字に変換するには、```ToLower()``` メソッドを使用します。これは、指定された文字列を全て小文字に変換した新しい文字列を返します。以下は、このメソッドを使用したコーディング例です。

```C#
string str = "Hello World!";
str = str.ToLower(); // "hello world!" が出力されます。
```

## 詳細情報：

文字列を小文字に変換する方法は、プログラミング言語によって異なります。C#では、この目的には```ToLower()``` メソッドを使用しますが、他の言語では別の方法で実装されている場合もあります。

また、正規表現を使用して文字列を小文字に変換する方法もあります。しかし、この方法は文字列の大量処理時やパフォーマンスを求められる場合には遅いとされています。

## 関連情報：

- C# ```ToLower()``` メソッドのドキュメンテーション：https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1
- 正規表現を使用した文字列操作の詳細：https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference