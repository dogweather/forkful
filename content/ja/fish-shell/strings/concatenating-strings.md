---
date: 2024-01-20 17:34:37.033535-07:00
description: "How to: (\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\u3001\
  \u53E4\u304F\u304B\u3089\u3042\u308B\u57FA\u672C\u7684\u306A\u30B3\u30FC\u30C7\u30A3\
  \u30F3\u30B0\u6280\u8853\u3067\u3059\u304C\u3001Fish Shell\u3067\u306F\u305D\u306E\
  \u66F8\u304D\u65B9\u304C\u4ED6\u306E\u30B7\u30A7\u30EB\u3068\u306F\u82E5\u5E72\u7570\
  \u306A\u308A\u307E\u3059\u3002\u4F8B\u3048\u3070\u3001Bash\u3067\u306F\u5909\u6570\
  \u3092\u30C0\u30D6\u30EB\u30AF\u30AA\u30FC\u30C8\u3067\u56F2\u3080\u5FC5\u8981\u304C\
  \u3042\u308A\u307E\u3059\u304C\u3001Fish\u3067\u306F\u5909\u6570\u5C55\u958B\u306B\
  \u30C0\u30D6\u30EB\u30AF\u30AA\u30FC\u30C8\u306F\u4E0D\u8981\u3067\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.506176-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\u3001\u53E4\u304F\
  \u304B\u3089\u3042\u308B\u57FA\u672C\u7684\u306A\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\
  \u6280\u8853\u3067\u3059\u304C\u3001Fish Shell\u3067\u306F\u305D\u306E\u66F8\u304D\
  \u65B9\u304C\u4ED6\u306E\u30B7\u30A7\u30EB\u3068\u306F\u82E5\u5E72\u7570\u306A\u308A\
  \u307E\u3059\u3002\u4F8B\u3048\u3070\u3001Bash\u3067\u306F\u5909\u6570\u3092\u30C0\
  \u30D6\u30EB\u30AF\u30AA\u30FC\u30C8\u3067\u56F2\u3080\u5FC5\u8981\u304C\u3042\u308A\
  \u307E\u3059\u304C\u3001Fish\u3067\u306F\u5909\u6570\u5C55\u958B\u306B\u30C0\u30D6\
  \u30EB\u30AF\u30AA\u30FC\u30C8\u306F\u4E0D\u8981\u3067\u3059."
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
