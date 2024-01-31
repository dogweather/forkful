---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:38:12.915451-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

category:             "Fish Shell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換することは、大文字を対応する小文字に変えることです。これはデータの標準化や、大文字と小文字を区別しない検索、ソートに使われます。

## How to: (方法)
Fish Shellでは、`string lower` コマンドを使って文字列を小文字に変換できます。

```Fish Shell
echo 'Fish SHELL is Fun!' | string lower
```

出力:

```
fish shell is fun!
```

## Deep Dive (深掘り)
文字列を小文字に変換する操作は、プログラマーにとって基本的です。歴史的にこれはUNIX系システムで小さな文字変換ツールとして始まりました。Fish Shellでは`string lower`コマンドがこの任務を担います。このコマンドはユニコードにも対応しているため、多国言語の大文字も小文字に変換可能です。代替としては、`tr` コマンドや `awk` の組み込み関数を使う方法もありますが、Fish Shell内蔵の`string lower`が最も簡単です。

## See Also (関連情報)
- [Fish Shell Documentation on `string`](https://fishshell.com/docs/current/cmds/string.html)
- [AWK Programming Language](https://www.gnu.org/software/gawk/manual/gawk.html)
