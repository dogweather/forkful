---
title:    "Elm: 文字列を小文字に変換する"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

変数の値を小文字に変換する必要性について、1-2文で説明します。

コード内で扱う文字列を整理するときに、大文字と小文字の区別があると混乱を招くことがあります。そのため、文や単語をすべて小文字に変換することで、コードをより一貫性のあるものにすることができます。

## やり方

Elmプログラミング言語で文字列を小文字に変換する方法は簡単です。下記のコードブロックを参考にしてください。

```Elm
import String

String.toLower "HELLO" -- Output: "hello"
```

ここでは、StringモジュールのtoLower関数を使用して、"HELLO"という文字列を小文字に変換しています。この関数は他のモジュールに比べると特に簡単なため、初心者でも簡単に使いこなすことができるでしょう。

次に、変数として定義した文字列を小文字に変換する場合はどうなるでしょうか。

```Elm
import String

myString = "WORLD"

String.toLower myString -- Output: "world"
```

上記のコードでは、変数myStringに代入された"WORLD"をtoLower関数に渡して、文字列を小文字に変換しています。これでコード内での大文字・小文字の区別をなくすことができます。

## 深堀り

文字列を小文字に変換する方法は単純に見えますが、内部的にはどのように処理されているのでしょうか。実際には、以下のようなアルゴリズムが使われています。

1. 最初に変換する文字列をUTF-16に変換する。
2. 変換後のUTF-16をUnicodeに変換する。
3. 最後に、Unicodeを小文字に変換する。

これにより、様々な言語や文字に対応した文字列の変換が可能になります。

## 参考リンク

 - [Elm Official Documentation on Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
 - [String.toLower source code on GitHub](https://github.com/elm/core/blob/1.0.5/src/String.elm#L149-L155)
 - [The Absolute Minimum Every Software Developer Absolutely, Positively Must Know About Unicode and Character Sets (No Excuses!)](http://www.joelonsoftware.com/articles/Unicode.html)

## 参考になる記事

 - [文字列操作について学ぼう！ - Qiita](https://qiita.com/nariaki3551/items/8f0d807d6d907b5f28f5) 
 - [Elmで文字列操作をマスターする！ - Qiita](https://qiita.com/corona/items/33c41a6552b249554171)