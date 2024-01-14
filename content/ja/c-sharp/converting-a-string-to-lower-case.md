---
title:                "C#: 「文字列を小文字に変換する」"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換するのはなぜでしょうか？これは、プログラミングにおいて、文字列の形式を統一するためによく行われる操作です。大文字と小文字が混在していると、データの比較や検索が困難になります。そのため、文字列を小文字に変換することで操作をシンプルにすることができます。

## 方法
下記のコード例を参考にして、C#で文字列を小文字に変換する方法をご紹介します。

```C#
string input = "Hello World";
string output = input.ToLower();

Console.WriteLine(output);

// 出力結果: hello world
```

上記の例では、`ToLower()`メソッドを使用して文字列を小文字に変換しています。このメソッドは、文字列を小文字に変換した新しい文字列を返すため、変数`output`に代入しています。そして`Console.WriteLine()`を使用して、変換された文字列をコンソールに表示しています。

もし、変換したい文字列が変数ではなく、ユーザーからの入力などであれば、`ToLower()`メソッドの代わりに`ToLowerInvariant()`メソッドを使用することをお勧めします。これは、異なる文化や言語の環境でも正しい変換が行われるため、より信頼性の高い方法です。

## ディープダイブ
C#の`ToLower()`メソッドは、内部で`CurrentCulture`というプロパティを使用して、現在のカルチャー（言語や地域の設定）に基づいて変換を行います。そのため、もしも独自の変換ルールを使用したい場合は、このプロパティを変更することで実現することができます。

また、このメソッドは文字列の先頭だけを小文字に変換する`ToLowerFirst()`メソッドや、特定の文字種のみを対象とする`ToLowerInvariant()`メソッドといったバリエーションも存在します。必要に応じて、適切なメソッドを使用することで、より細かい文字列操作が可能になります。

## See Also
- [C#で文字列を大文字に変換する方法](https://example.com/article2)
- [C#で文字列の長さを取得する方法](https://example.com/article3)