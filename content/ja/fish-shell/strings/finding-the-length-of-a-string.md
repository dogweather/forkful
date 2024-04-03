---
date: 2024-01-20 17:47:22.198873-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.718645-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
