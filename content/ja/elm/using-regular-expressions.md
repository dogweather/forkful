---
title:                "Elm: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用するのは、テキストの検索や置換を行うために非常に便利です。また、特定のパターンを持つ文字列を検出する場合や、バリデーションを行う場合にも役立ちます。

## 使い方

```elm
import Regex exposing (..)

-- 文字列の検索
Regex.contains (regex "elm") "I love Elm!" -- True

-- 文字列の置換
Regex.replace (regex "Elm") (\_ -> "JavaScript") "I love Elm!" -- "I love JavaScript!"

-- 文字列の分割
Regex.split (regex "[, ;]") "apple, banana; orange" -- ["apple","banana","orange"]
```

## 深堀り

正規表現を使用する際に覚えておくべきポイントはいくつかあります。まず、文字クラスを使用することで、文字列の範囲を指定することができます。例えば、[a-z]と指定することで、小文字のアルファベットを表すことができます。また、正規表現内で使用される特殊な文字を文字として扱いたい場合は、バックスラッシュを使用することでエスケープすることができます。

## 参考

- [Elm公式ドキュメント](https://guide.elm-lang.jp/)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
- [regexパッケージのドキュメント](https://package.elm-lang.org/packages/elm/regex/latest/)