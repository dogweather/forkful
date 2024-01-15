---
title:                "部分文字列の抽出"
html_title:           "Elm: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ使うのか

文字列から部分文字列を抽出するのは、特定の文字や単語を検索したり、文書を分析したりする際に非常に役立ちます。 Elmでこれを行う方法を学ぶことで、より効率的なプログラムが書けるようになります。

## 方法

まずは、使用する文字列を定義してみましょう。例えば、"こんにちは、世界！"という文字列があるとします。次に、この文字列から"こんにちは"という部分文字列を抽出してみましょう。

```elm
import String exposing (left)

left 5 "こんにちは、世界！" -- 出力: "こんにちは"
```

これで、指定した文字数分の部分文字列を抽出することができました。同様に、`right`関数を使用することで、文字列の末尾から部分文字列を抽出することができます。

また、特定の文字や単語を検索する場合は、`contains`関数を使用します。例えば、"こんにちは、世界！"から"こんにちは"が含まれているかどうか調べるには以下のようになります。

```elm
import String exposing (contains)

contains "こんにちは、世界！" "こんにちは" -- 出力: True
```

## ディープダイブ

部分文字列を抽出する際には、使用する関数によって挙動が異なります。例えば、`left`関数は指定した文字数分の部分文字列を抽出するだけでなく、指定した文字数よりも元の文字列が短い場合には元の文字列全体を返します。一方、`leftJustify`関数は指定した文字数よりも元の文字列が短い場合には空白文字で埋めて返します。このように、使用する関数を理解することで、より柔軟に部分文字列を抽出することができます。

## 関連リンク

- [Official Elm Website](https://elm-lang.org/)
- [Elm Japan Community Page](https://elmjapan.org/)
- [Elm on Reddit](https://www.reddit.com/r/elm/)