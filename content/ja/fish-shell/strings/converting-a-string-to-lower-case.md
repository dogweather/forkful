---
date: 2024-01-20 17:38:12.915451-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3053\u3068\u306F\u3001\u5927\u6587\u5B57\u3092\u5BFE\u5FDC\u3059\u308B\u5C0F\u6587\
  \u5B57\u306B\u5909\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u30C7\
  \u30FC\u30BF\u306E\u6A19\u6E96\u5316\u3084\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\
  \u5B57\u3092\u533A\u5225\u3057\u306A\u3044\u691C\u7D22\u3001\u30BD\u30FC\u30C8\u306B\
  \u4F7F\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.822663
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3053\u3068\u306F\u3001\u5927\u6587\u5B57\u3092\u5BFE\u5FDC\u3059\u308B\u5C0F\u6587\
  \u5B57\u306B\u5909\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u30C7\
  \u30FC\u30BF\u306E\u6A19\u6E96\u5316\u3084\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\
  \u5B57\u3092\u533A\u5225\u3057\u306A\u3044\u691C\u7D22\u3001\u30BD\u30FC\u30C8\u306B\
  \u4F7F\u308F\u308C\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
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
