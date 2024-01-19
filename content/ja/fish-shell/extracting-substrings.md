---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列抽出とは、大きな文字列から特定の部分を取り出す技術です。プログラマーがこれを行う理由は、必要なデータのみを操作したり、特定の情報を検索したりするためです。

## どうやって:

Fish Shellでは、`string sub`コマンドを使用して文字列を抽出します。

例えば、以下のコードは、"Hello, world!"から"world"を抽出します。

```fish
set string "Hello, world!"
string sub -s 8 -l 5 $string
```

このコードの出力は次のようになります：

```fish
world
```

## ディープダイブ:

Fish Shellの `string sub`コマンドは、文字列の指定された範囲を抽出するための便利な道具です。しかしながら、より複雑な条件や状況下では、正規表現や`string match`を使用した方が良い場合もあります。

また、Fish Shellでは他にもさまざまなテキスト処理コマンドが用意されており、`string replace`, `string split`, `string join`, 等を組み合わせることで、文字列操作の可能性をさらに広げることができます。

## 参考資料:

Fish Shellの公式ドキュメンテーションは非常に詳しいので、さらに学びたい方は以下のリンクをチェックしてみてください:

1. [Fish Shell 文字列操作](https://fishshell.com/docs/current/commands.html#string)
2. [Fish Shell コマンドライン使い方](https://fishshell.com/docs/current/tutorial.html)
3. [Fish Shell 正規表現マッチング](https://fishshell.com/docs/current/commands.html#string-match)