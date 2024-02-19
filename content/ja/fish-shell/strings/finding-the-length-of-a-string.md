---
aliases:
- /ja/fish-shell/finding-the-length-of-a-string/
date: 2024-01-20 17:47:22.198873-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8ABF\u3079\u308B\u306E\u306F\
  \u3001\u305D\u306E\u6587\u5B57\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3084\
  \u5165\u529B\u306E\u691C\u8A3C\u306B\u3053\u306E\u60C5\u5831\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.299103
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8ABF\u3079\u308B\u306E\u306F\
  \u3001\u305D\u306E\u6587\u5B57\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3084\
  \u5165\u529B\u306E\u691C\u8A3C\u306B\u3053\u306E\u60C5\u5831\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
文字列の長さを調べるのは、その文字数を数えることです。プログラマーはデータ処理や入力の検証にこの情報を使用します。

## How to: (やり方)
```Fish Shell
# 文字列の長さを取得する
set string "こんにちは"
echo (string length $string)
```
```
5
```

## Deep Dive (深堀り)
Fish Shellの `string length` コマンドは文字列の文字数を返す。これはUnicode文字を正しく数える。以前では、文字列の長さを数えるのは `wc -m` などの外部コマンドに依存していたが、内部コマンドを使うことで速度が向上する。他のシェルスクリプト言語では、例えば Bash では `${#string}` 構文を使うが、Fishはよりシンプルで直感的な `string length` を提供する。

## See Also (参照)
- Fish documentation on string manipulation: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Fish tutorial for beginners: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- Unicode Standard: [https://unicode.org/standard/standard.html](https://unicode.org/standard/standard.html)
