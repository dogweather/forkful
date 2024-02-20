---
date: 2024-01-20 17:45:02.721397-07:00
description: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u629C\u304D\u51FA\u3059\
  \u3063\u3066\u3044\u3046\u306E\u306F\u6587\u5B57\u5217\u306E\u4E00\u90E8\u3092\u5207\
  \u308A\u53D6\u308B\u3053\u3068\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u306A\
  \u305C\u305D\u308C\u3092\u3059\u308B\u304B\uFF1F\u30C7\u30FC\u30BF\u3092\u6271\u3046\
  \u6642\u306B\u7279\u5B9A\u306E\u60C5\u5831\u3060\u3051\u304C\u5FC5\u8981\u3060\u304B\
  \u3089\u3060\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.481668
model: gpt-4-1106-preview
summary: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u629C\u304D\u51FA\u3059\
  \u3063\u3066\u3044\u3046\u306E\u306F\u6587\u5B57\u5217\u306E\u4E00\u90E8\u3092\u5207\
  \u308A\u53D6\u308B\u3053\u3068\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u306A\
  \u305C\u305D\u308C\u3092\u3059\u308B\u304B\uFF1F\u30C7\u30FC\u30BF\u3092\u6271\u3046\
  \u6642\u306B\u7279\u5B9A\u306E\u60C5\u5831\u3060\u3051\u304C\u5FC5\u8981\u3060\u304B\
  \u3089\u3060\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
サブストリングを抜き出すっていうのは文字列の一部を切り取ること。プログラマーはなぜそれをするか？データを扱う時に特定の情報だけが必要だからだ。

## How to: (方法)
Bashではサブストリングは簡単に抜き出せる。例と出力を見てみよう。

```Bash
# Example 1: 文字列から部分を抜き出す
string="こんにちは、世界！"
echo ${string:7:3}

# 出力: 世
```

```Bash
# Example 2: 文字列の後ろから部分を抜き出す
string="ファイル名: report-2023.txt"
echo ${string: -4}

# 出力: .txt
```

## Deep Dive (深掘り)
Bashでのサブストリングの抽出は、Bash自体の出現 (1989年) 以来、スクリプトの基本の一部。`expr` や `cut` のような外部コマンドを使う代わりに、Bashの組み込み機能を使用すると効率が良い。変数を操作する方法は、プラットフォームに依存せず、余分なプロセスを起動する必要がない。

始点と長さを指定する方法の他に、パターンマッチングを使ってサブストリングを取り出すやり方もある：

```Bash
# Example 3: パターンマッチングでサブストリングを抜き出す
string="重要：このメールは機密事項を含んでいます。"
echo ${string#重要：}

# 出力: このメールは機密事項を含んでいます。
```

基本的に、サブストリングを抜き出すときは`${変数名:オフセット:長さ}`の構文を使う。オフセットをマイナスにすることで、文字列の最後からカウントもできる。

## See Also (関連情報)
- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)

これらのリンクには、Bashでの文字列操作についてのさらに詳しい情報が詰まっている。
