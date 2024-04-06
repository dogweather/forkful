---
date: 2024-01-20 17:47:22.198873-07:00
description: "How to: (\u3084\u308A\u65B9) Fish Shell\u306E `string length` \u30B3\
  \u30DE\u30F3\u30C9\u306F\u6587\u5B57\u5217\u306E\u6587\u5B57\u6570\u3092\u8FD4\u3059\
  \u3002\u3053\u308C\u306FUnicode\u6587\u5B57\u3092\u6B63\u3057\u304F\u6570\u3048\u308B\
  \u3002\u4EE5\u524D\u3067\u306F\u3001\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6570\
  \u3048\u308B\u306E\u306F `wc -m`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.505114-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Fish Shell\u306E `string length` \u30B3\u30DE\u30F3\
  \u30C9\u306F\u6587\u5B57\u5217\u306E\u6587\u5B57\u6570\u3092\u8FD4\u3059\u3002\u3053\
  \u308C\u306FUnicode\u6587\u5B57\u3092\u6B63\u3057\u304F\u6570\u3048\u308B\u3002\u4EE5\
  \u524D\u3067\u306F\u3001\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6570\u3048\u308B\
  \u306E\u306F `wc -m` \u306A\u3069\u306E\u5916\u90E8\u30B3\u30DE\u30F3\u30C9\u306B\
  \u4F9D\u5B58\u3057\u3066\u3044\u305F\u304C\u3001\u5185\u90E8\u30B3\u30DE\u30F3\u30C9\
  \u3092\u4F7F\u3046\u3053\u3068\u3067\u901F\u5EA6\u304C\u5411\u4E0A\u3059\u308B\u3002\
  \u4ED6\u306E\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\u8A9E\u3067\u306F\
  \u3001\u4F8B\u3048\u3070 Bash \u3067\u306F `${#string}` \u69CB\u6587\u3092\u4F7F\
  \u3046\u304C\u3001Fish\u306F\u3088\u308A\u30B7\u30F3\u30D7\u30EB\u3067\u76F4\u611F\
  \u7684\u306A `string length` \u3092\u63D0\u4F9B\u3059\u308B\u3002"
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
