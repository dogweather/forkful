---
date: 2024-01-20 17:34:37.033535-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.719856-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

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
