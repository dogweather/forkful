---
title:    "Elm: テキストの検索と置換"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

プログラミングにおいて、テキストを検索して置き換えることは非常に重要です。例えば、大規模なコードベースで同じ単語やフレーズを一括で変更する必要がある場合など、テキストの検索と置き換えは時短になります。また、間違ったスペルを修正するためにも便利です。

## How To

テキストの検索と置き換えは、Elmのテキスト処理機能を使用して簡単に行うことができます。まず、検索したいテキストを指定して、その後に置き換えたいテキストを指定します。

```Elm
import String

replaceText : String -> String -> String -> String
replaceText oldText newText text =
    String.replace oldText newText text
```

上記の例では、`replaceText`という関数を定義し、引数として古いテキスト、新しいテキスト、および元のテキストを受け取ります。`String.replace`関数を使用して、古いテキストを新しいテキストに置き換えるように命令します。

```Elm
replaceText "Good" "Great" "That was a good movie."
-- Output: "That was a great movie."
```

このように、簡単にテキストの検索と置き換えができます。

## Deep Dive

Elmでは、「Regex」モジュールを使用することで、より高度なテキストの検索と置き換えを行うことができます。これは、正規表現を使用してパターンを指定することで、複雑なテキストの検索と置き換えが可能になります。

例えば、「正規表現が何であったとしても、いずれかの数字を含むテキスト」を検索し、置き換えるように命令するコードは以下のようになります。

```Elm
import Regex

changeNumbers : String -> String
changeNumbers text =
    Regex.replace Regex.All "numbers" ((\ number -> "digits") . List.head) text
```

この例では、`changeNumbers`関数を定義し、引数として受け取ったテキスト内のすべての数字を「numbers」から「digits」に置き換えるように命令します。`(\ number -> "digits") . List.head`という式は、数字のリストの最初の要素のみを「digits」に置き換える関数を定義しています。

```Elm
changeNumbers "123 numbers in this text."
-- Output: "123 digits in this text."
```

より複雑なテキストの検索と置き換えを行う際には、正規表現を学ぶことでよりパワフルなコードを書くことができるようになります。

## See Also

- Elmのテキスト処理機能についての詳細は、公式ドキュメントを参照してください。[Text Processing](https://guide.elm-lang.org/effects/text.html)
- 正規表現について学ぶには、[RegexOne](https://regexone.com/)や[Regex Golf](https://alf.nu/RegexGolf)などのオンラインゲームが役立ちます。