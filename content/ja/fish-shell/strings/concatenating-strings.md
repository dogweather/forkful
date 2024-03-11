---
date: 2024-01-20 17:34:37.033535-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u4E8C\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u7D50\u5408\u3057\u3066\u4E00\u3064\u306E\u6587\
  \u5B57\u5217\u3092\u4F5C\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u304C\u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\
  \u30BF\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u306E\u751F\u6210\u3001\u307E\u305F\u306F\u30B3\u30FC\u30C9\u5185\u3067\u52D5\u7684\
  \u306B\u6587\u5B57\u5217\u3092\u64CD\u4F5C\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\
  \u304B\u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:16.268902-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u4E8C\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u7D50\u5408\u3057\u3066\u4E00\u3064\u306E\u6587\
  \u5B57\u5217\u3092\u4F5C\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u304C\u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\
  \u30BF\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u306E\u751F\u6210\u3001\u307E\u305F\u306F\u30B3\u30FC\u30C9\u5185\u3067\u52D5\u7684\
  \u306B\u6587\u5B57\u5217\u3092\u64CD\u4F5C\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\
  \u304B\u3089\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結とは、二つ以上の文字列を結合して一つの文字列を作ることです。プログラマーがこれを行う理由は、データのフォーマット、メッセージの生成、またはコード内で動的に文字列を操作する必要があるからです。

## How to: (方法)
```Fish Shell
# 単純な連結
set greeting "こんにちは、"
set name "世界！"
echo $greeting$name
# 出力: こんにちは、世界！

# 変数と文字列を連結
set age 100
echo "私は" $age "歳です。"
# 出力: 私は 100 歳です。

# 複数の変数を連結
set city "京都"
set weather "晴れ"
echo $city "の天気は" $weather "です。"
# 出力: 京都の天気は晴れです。
```

## Deep Dive (深掘り)
文字列の連結は、古くからある基本的なコーディング技術ですが、Fish Shellではその書き方が他のシェルとは若干異なります。例えば、Bashでは変数をダブルクオートで囲む必要がありますが、Fishでは変数展開にダブルクオートは不要です。

他の方法としては`string`コマンドを利用する方法がありますが、短い文字列には上記のような直接的な連結が効率的です。Fishの場合、`string`コマンドはより複雑な文字列操作で力を発揮します。

また、Fishのバージョン3.0.0からは、変数の間に空白を挿入したくない場合、`{$var}`と書くことで連結できるようになりましたが、シンプルな連結であれば変数名を続けて書くだけで十分です。

## See Also (参照)
- [Fish Documentation on String Manipulation](https://fishshell.com/docs/current/index.html#syntax-variable-expansion)
- [Fish Tutorial](https://fishshell.com/docs/current/tutorial.html)
