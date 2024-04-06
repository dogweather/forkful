---
date: 2024-01-20 17:38:12.915451-07:00
description: "How to: (\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001`string lower` \u30B3\
  \u30DE\u30F3\u30C9\u3092\u4F7F\u3063\u3066\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.500761-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001`string lower` \u30B3\u30DE\u30F3\
  \u30C9\u3092\u4F7F\u3063\u3066\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\
  \u63DB\u3067\u304D\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
