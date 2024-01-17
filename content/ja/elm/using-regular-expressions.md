---
title:                "「正規表現を使用する」"
html_title:           "Elm: 「正規表現を使用する」"
simple_title:         "「正規表現を使用する」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何？なぜ？

正規表現を使用すると、特定のパターンに一致する文字列を検索したり、置換したりすることができます。プログラマーは、データの有用な情報を抽出したり、文字列の整形を行ったりする際に、正規表現を使用します。

## 方法：

```
Elm.regex "world" "Hello, world!"
-- Output: Just "world"
    
Elm.regex "zip" "zip code"
-- Output: Just "zip"
```

上記の例では、```Elm.regex```関数を使用して、文字列内の指定したパターンに一致する部分を抽出しています。

## 深堀り：

正規表現は、1950年代から広く使用されているテキスト処理の術で、プログラミング言語によって実装が異なります。代替手段として、パターンマッチングや文字列操作があります。Elmでは、```Regex```モジュールを使用して正規表現を扱うことができます。

## 関連情報：

- [Elm公式ドキュメント: 正規表現](https://guide.elm-lang.org/effects/text_regex.html)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)