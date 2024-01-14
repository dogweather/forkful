---
title:                "Elm: テキストの検索と置換"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換を行う理由は、コードやドキュメントを自動的に更新したり、大規模なプロジェクトの管理を容易にするためです。

## 方法

検索と置換の最も基本的な方法は、`String.replace`を使用することです。例えば、次のように使用することができます。

```Elm
string = "こんにちは、世界！"
newString = String.replace "こんにちは" "Hello" string
```

結果は`"Hello、世界！"`となります。また、より複雑なパターンの検索と置換を行うには、正規表現パターンを使用することができます。例えば、次のように使用することができます。

```Elm
string = "abc123xyz"
newString = Regex.replace Regex.All (Regex.regex "[0-9]+") (\_ -> "456") string
```

結果は`黙示録345xyz`となります。

## ディープダイブ

正規表現を使用することで、より複雑な条件での検索と置換が可能になります。また、Elmの標準ライブラリだけでなく、外部のライブラリを使用することで、より高度な検索と置換を行うこともできます。

## 参考リンク

- [Elm公式ドキュメント - String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm公式ドキュメント - Regex](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [elm-regex-test](https://package.elm-lang.org/packages/debuggler/elm-regex-test/latest/)