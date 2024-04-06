---
date: 2024-01-20 17:45:02.721397-07:00
description: "How to: (\u65B9\u6CD5) Bash\u3067\u306F\u30B5\u30D6\u30B9\u30C8\u30EA\
  \u30F3\u30B0\u306F\u7C21\u5358\u306B\u629C\u304D\u51FA\u305B\u308B\u3002\u4F8B\u3068\
  \u51FA\u529B\u3092\u898B\u3066\u307F\u3088\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.190092-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Bash\u3067\u306F\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\
  \u306F\u7C21\u5358\u306B\u629C\u304D\u51FA\u305B\u308B\u3002\u4F8B\u3068\u51FA\u529B\
  \u3092\u898B\u3066\u307F\u3088\u3046\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
