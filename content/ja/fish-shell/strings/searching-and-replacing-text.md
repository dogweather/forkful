---
date: 2024-01-20 17:57:59.278779-07:00
description: "How to: (\u65B9\u6CD5) Fish Shell\u3067\u306F`string`\u30C4\u30FC\u30EB\
  \u304C\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u305F\u3081\u306B\u7528\u610F\u3055\u308C\
  \u3066\u3044\u307E\u3059\u3002\u53E4\u3044\u30B7\u30A7\u30EB\u3067\u306F`sed`\u3084\
  `awk`\u304C\u4E3B\u6D41\u3067\u3057\u305F\u304C\u3001Fish\u306F\u3088\u308A\u76F4\
  \u89B3\u7684\u306B\u4F7F\u3048\u308B\u30B3\u30DE\u30F3\u30C9\u3092\u63D0\u4F9B\u3057\
  \u307E\u3059\u3002\u4F8B\u3048\u3070\u3001`string\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.498569-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Fish Shell\u3067\u306F`string`\u30C4\u30FC\u30EB\u304C\u6587\
  \u5B57\u5217\u64CD\u4F5C\u306E\u305F\u3081\u306B\u7528\u610F\u3055\u308C\u3066\u3044\
  \u307E\u3059\u3002\u53E4\u3044\u30B7\u30A7\u30EB\u3067\u306F`sed`\u3084`awk`\u304C\
  \u4E3B\u6D41\u3067\u3057\u305F\u304C\u3001Fish\u306F\u3088\u308A\u76F4\u89B3\u7684\
  \u306B\u4F7F\u3048\u308B\u30B3\u30DE\u30F3\u30C9\u3092\u63D0\u4F9B\u3057\u307E\u3059\
  \u3002\u4F8B\u3048\u3070\u3001`string replace`\u306F\u76F4\u63A5\u7684\u306A\u547D\
  \u540D\u3067\u4F55\u3092\u3059\u308B\u304B\u660E\u767D\u3067\u3059\u3002\u5B9F\u88C5\
  \u9762\u3067\u306F\u3001Fish\u306FUTF-8\u30A8\u30F3\u30B3\u30FC\u30C7\u30A3\u30F3\
  \u30B0\u306E\u6587\u5B57\u5217\u306B\u5BFE\u5FDC\u3057\u3001\u8A2D\u8A08\u304C\u5358\
  \u7D14\u3067\u308F\u304B\u308A\u3084\u3059\u3044\u3067\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to: (方法)
```Fish Shell
# 文字列 'fish' を 'shark' に置換する
echo "I love fish tacos" | string replace "fish" "shark"
# 出力: I love shark tacos

# ファイル内の全 'fish' を 'shark' に置換
string replace -a -i "fish" "shark" file.txt
# file.txt 内の全ての 'fish' が 'shark' に置換される
```

## Deep Dive (深い潜水)
Fish Shellでは`string`ツールが文字列操作のために用意されています。古いシェルでは`sed`や`awk`が主流でしたが、Fishはより直観的に使えるコマンドを提供します。例えば、`string replace`は直接的な命名で何をするか明白です。実装面では、FishはUTF-8エンコーディングの文字列に対応し、設計が単純でわかりやすいです。

## See Also (関連情報)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [GNU Sed Manual](https://www.gnu.org/software/sed/manual/sed.html) - 別の検索・置換ツール
- [AWK Programming Language](https://www.gnu.org/software/gawk/manual/gawk.html) - テキスト処理のためのプログラム言語
