---
title:    "Elm: 正規表現の使用"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ？

回答:
正規表現を使用する利点は多岐にわたります。例えば、文字列のパターンを簡単に検索、置換、抽出することができます。また、入力されたデータの妥当性をチェックすることもできます。それでは、Elmで正規表現をどのように使うか見ていきましょう。

## 使い方

```Elm
import Regex exposing (..)

-- 文字列のパターンに一致するかどうかを確認する例
Regex.contains (regex "elm") "Elm is a functional programming language" -- 結果: True

-- 文字列を一致するパターンで分割する例
Regex.split (regex ":") "A:1:2:3" -- 結果: ["A", "1", "2", "3"]

-- 文字列内のパターンを置換する例
Regex.replace (regex "[^a-zA-Z0-9]+") (always "-") "Hello, World!" -- 結果: "Hello-World"

-- 入力されたデータの妥当性をチェックする例 (電話番号のフォーマットをチェック)
Regex.contains (regex "^\\d{3}-\\d{4}-\\d{4}$") "090-1234-5678" -- 結果: True
```

## 深堀り

正規表現を使用する際には、パターンを理解し、適切に表現することが重要です。例えば、"^\\d{3}[- ]\\d{4}[- ]\\d{4}$"というパターンは、電話番号のフォーマットを検証することができます。このパターンは、3桁の数字の後に「-」または「 」（スペース）が続き、その後4桁の数字の後に再び「-」または「 」（スペース）が続き、最後にもう一度4桁の数字が続くという意味です。正規表現のパターンはさまざまな方法で検索、置換、抽出することができるので、より複雑なパターンを学ぶことも可能です。

## 参考リンク

- [正規表現についてのElmドキュメンテーション](https://package.elm-lang.org/packages/elm/regex/latest/)
- [正規表現チュートリアル](https://regexone.com/)
- [正規表現を使用した文字列操作のヒント](https://medium.com/free-code-camp/effective-string-manipulation-with-elm-3bf3b2ec56b6)