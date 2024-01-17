---
title:                "文字列を連結する"
html_title:           "Haskell: 文字列を連結する"
simple_title:         "文字列を連結する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

# 何が & なぜ?
文字列の結合とは何かというのは、要するに複数の文字列を一緒にくっつけることです。プログラマーは文字列を結合することで、複数の文字列を１つのものとして扱うことができます。これは、テキスト処理やデータ操作など、さまざまなプログラミングのタスクで役立つ重要な機能です。

## 方法:
``` Haskell
-- ２つの文字列を結合する
concatStrings :: String -> String -> String
concatStrings str1 str2 = str1 ++ str2

-- 結合された文字列を出力する
main = putStrLn (concatStrings "Hello " "World!")
```
```
出力: Hello World!
```

## 詳しく見ていく:
### 歴史的な背景:
文字列の結合は古くからプログラミング言語に存在しており、初期の言語でも利用されてきました。しかし、Haskellのような関数型プログラミング言語では、文字列を結合する際に演算子ではなく関数として扱うことが一般的です。

### 代替手段:
文字列の結合は、他の代替手段に比べて非常に効率的な方法です。しかし、文字列の不変性のため、大量の文字列を結合する際にはパフォーマンスの低下が問題となることがあります。そのため、Haskellでは文字列の代わりにリストを使用することもできます。

### 実装の詳細:
Haskellでは、文字列を結合するための演算子は(++)です。この演算子は、リストを結合する際にも使用されます。Haskellの文字列は実際には文字のリストとして表現されるため、文字列の結合はリストの結合と同じ処理となります。

## 関連リンク:
- [Haskellの文書: 文字列の結合](https://www.haskell.org/tutorial/strings.html#concatenation)
- [HaskellWiki: 文字列の結合](https://wiki.haskell.org/String_concatenation)