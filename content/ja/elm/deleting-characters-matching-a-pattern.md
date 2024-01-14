---
title:                "Elm: パターンに一致する文字の削除"
simple_title:         "パターンに一致する文字の削除"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

プログラミング言語の一つであるElmを使用して、文字列の中から特定のパターンにマッチする文字を削除することは、コードの効率性を高めるために役立ちます。この記事では、その理由について詳しく説明します。

## 方法

プログラミングにおいて、文字の削除は非常に一般的な作業です。しかし、特定のパターンにマッチする文字を削除することは、それぞれのプログラミング言語によって異なったアプローチが求められます。Elmを使用する場合、以下のようなコードを使用することで、簡単に文字を削除することができます。

```Elm
import String exposing (..)

-- 文字列から特定のパターンにマッチする文字を削除する
deleteCharPattern : String -> String -> String
deleteCharPattern pattern str =
  let
    -- Regexモジュールを使用して、マッチした文字を削除する
    regex = Regex.replace (Regex.regex pattern) (always " ")
  in
    -- 置換後の文字列を返す
    Regex.replace regex (always str)
```

例えば、以下のような文字列から、数字を削除したい場合、上記の関数を使用することができます。

`deleteCharPattern "[0-9]" "1abc23def45"`

出力は以下のようになります。

`" abcdef"`

## ディープダイブ

上記のコード例では、文字列内の指定したパターン全ての文字を削除していますが、場合によっては特定の位置にマッチする文字を削除したい場合もあります。その場合は、以下のような関数を使用することができます。

```Elm
import String exposing (..)

-- 文字列の特定の位置にマッチする文字を削除する
deleteCharAt : Int -> String -> String
deleteCharAt index str =
  -- takeとdropを使用して、指定した位置の文字を取り除く
  take index str ++ drop (index + 1) str
```

例えば、以下のような文字列から、3番目の位置にある文字を削除したい場合、上記の関数を使用することができます。

`deleteCharAt 2 "abcde"` 

出力は以下のようになります。

`"abde"`

## もっと詳しく

Elmには、テキストの処理を補助するための様々なモジュールがあります。この記事では紹介しきれなかったRegex以外にも、Stringモジュールを使用する方法などがあります。ぜひ、さらに深く学んでみてください。

## 参考リンク

- [Elm公式ドキュメント - Stringモジュール](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm公式ドキュメント - Regexモジュール](https://package.elm-lang.org/packages/elm/regex/latest/)