---
title:                "パターンに一致する文字を削除する"
html_title:           "Haskell: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
文字を削除するパターンにマッチする文字を削除することに注意を払う理由は何ですか？

文字を削除する必要がある特定の状況があるかもしれません。たとえば、入力データから特定の文字を排除したい場合などです。または、テキストから特定のパターンを削除したい場合もあります。

## 方法
Haskellを使用して、指定したパターンにマッチする文字を削除する方法を説明します。

```
import Data.List

deleteMatching :: String -> String
deleteMatching str = delete 'a' str
```

上記の例では、`delete`関数を使用して文字列から`a`の文字を削除しています。

サンプル入力： `"Haskell is awesome!"`

サンプル出力： `"Hskell is wesome!"`

## ディープダイブ
Haskellでは、リストや文字列を操作するための多くの便利な関数が用意されています。その中の1つが`delete`関数です。この関数は、指定した要素をリストから削除することができます。`delete`関数は、文字列にも使用することができ、指定した文字を文字列から削除することができます。また、文字列の代わりにリストを使用することもできます。

Haskellの強力なパターンマッチング機能を使えば、より特定のパターンにマッチする文字を削除することが可能です。パターンマッチングについては、別の記事で詳しく説明しています。

## See Also
- [Haskellのパターンマッチングについて](https://example.com/pattern-matching-haskell)
- [Haskellのリスト操作関数一覧](https://example.com/list-functions-haskell)
- [Haskellの文字列操作関数一覧](https://example.com/string-functions-haskell)