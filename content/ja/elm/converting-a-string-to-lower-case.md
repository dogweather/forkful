---
title:                "「文字列を小文字に変換する」"
html_title:           "Elm: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何をし、なぜ?

文字列を小文字に変換することは、プログラマーにとって非常に重要なタスクです。これを行う主な理由は、データの整合性を保つためです。例えば、ユーザーから入力された文字列が大文字であった場合でも、データベースに保存される前に小文字に変換することで、同じ文字列であってもデータベース内の検索がスムーズに行われるようになります。

## 方法:

Elmでは、文字列を小文字に変換するために `String.toLower` 関数を使用します。以下の例をご覧ください。

```Elm
import String exposing (toLower)

name = "ELM PROGRAMMING"
lowercaseName = toLower name

-- output: "elm programming"
```

## さらに詳しく:

### 歴史的背景:

文字列を小文字に変換するというアイディアは、古くから存在していました。プログラム言語やデータベースの中で、文字列を比較する際に大文字と小文字を区別しないという考え方は、効率的なデータ処理の重要な要素となっています。

### 代替手段:

Elmでは `String.toLower` 関数以外にも、文字列を小文字に変換するための様々なツールがあります。例えば、正規表現やパターンマッチングを使用する方法もあります。

### 実装の詳細:

Elmで `String.toLower` 関数を使用する際、内部的にはUTF-8コードポイントに基づいて文字列を変換しています。このため、言語や文字系によらず正確な変換が行われることが保証されています。

## 関連情報:

- [Elm Documentation: Strings](https://package.elm-lang.org/packages/elm-lang/core/latest/String)
- [Elm Guide: Strings](https://guide.elm-lang.org/strings/)