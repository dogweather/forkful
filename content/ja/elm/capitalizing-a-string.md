---
title:                "Elm: 文字列の大文字化"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

なぜ文字列を大文字化するのか、ご存知ですか？単純な作業のように思えますが、実は意味があります。今回はElmプログラミング言語で文字列を大文字化する方法についてご紹介します。

## どのように

まずは、文字列を大文字化する方法をご紹介します。以下のコードを参考にしてください。

```Elm
String.toUpper "hello" --出力は "HELLO" になります
```

ここで、"String.toUpper"は文字列を大文字化する関数であり、引数に変換したい文字列を入れます。この例では、"hello"を大文字に変換しています。

また、文字列を小文字に変換する関数もあります。

```Elm
String.toLower "WORLD" --出力は "world" になります
```

文字列を大文字化するだけでなく、一部の文字だけを大文字に変換する方法もあります。例えば、最初の文字を大文字にしたい場合は以下のようにします。

```Elm
String.capitalize "hello" --出力は "Hello" になります
```

また、最初の2文字を大文字にしたい場合も同じように行えます。

```Elm
String.uppercaseFirst "hello" --出力は "HeLLO" になります
```

## ディープダイブ

さらに深く、文字列を大文字化する際の動作について見ていきましょう。文字列を大文字化する際、Elmでは文字列の長さやUnicode文字に対する処理が行われます。このように、プログラミングの観点から複雑に処理されるため、文字列を大文字化する際には単純な作業に感じられるかもしれませんが、実は多くの処理が行われています。

## 関連記事

- [Elm公式ドキュメント](https://guide.elm-lang.org/)
- [文字列を大文字化する方法](https://qiita.com/miyadesu1123/items/0673ae83f6a0446292dc)
- [Unicodeについて](https://www.w3.org/International/questions/qa-what-is-unicode)