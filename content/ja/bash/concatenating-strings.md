---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# Bashプログラミング：文字列の連結

## 何と、なぜ？

文字列の連結は、1つ以上の文字列を1つに結合するプロセスのことです。これは、情報生成または動的コード生成などのためによく使用されます。

## 使い方：
Bashでの文字列連結は直感的で簡単です。変数内に存在する文字列を直接連結することができます。例を見てみましょう:

```Bash
string1="こんにちは、"
string2="世界！"
greeting="$string1$string2"
echo $greeting
```
このコードは "こんにちは、世界！" を出力します。

## 深掘り：

文字列の連結は、古いUNIXのシェルスクリプト言語から継承された機能です。Bashやその他の現代のシェルスクリプト言語は、この基本的な機能を引き継ぎ、改善しました。

また、連結の代わりに `printf` 関数を使用することも可能です。

```Bash
string1="こんにちは、"
string2="世界！"
printf -v greeting "%s%s" "$string1" "$string2"
echo $greeting
```
このコードも "こんにちは、世界！" を出力します。

しかし、`printf` 関数は連結よりも多くの動作を可能にします。例えば、変数を特定の形式に整形したり、複数の異なる文字列を一度に連結したりできます。

## 参考文献：

1. Bash 文字列操作ガイド: https://www.tldp.org/LDP/abs/html/string-manipulation.html
2. Bash Scripting チュートリアル: https://ryanstutorials.net/bash-scripting-tutorial/bash-string-manipulation.php
3. Unix ＆ Linux Stack Exchangeのスレッド、 "How to concatenate strings in bash": https://unix.stackexchange.com/questions/63923/how-to-concatenate-strings-in-bash