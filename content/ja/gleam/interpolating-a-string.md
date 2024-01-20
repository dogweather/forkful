---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列補間とは、文字列中に変数を挿入する方法です。これを行う主な理由は一貫性と見通しの良さを確保するためです。

## やり方：

文字列補間を使ってみましょう。

```Gleam 
let name = "Yamada"
let hello = "こんにちは、{name}さん"
IO.println(hello)
```

出力結果:

```
こんにちは、Yamadaさん
```

## より深く：

1. 詠み込みの歴史的背景: 古いプログラミング言語では、文字列と変数を連結するために '+' オペレータを使用していました。でも、これは扱いが難しく、エラーを起こしやすいです。幸い、現代のプログラミング言語ではこの問題を解決するために文字列補間があります。

2. 代替案: 場合によっては、文字列の連結や、フォーマット指定子を使用した `printf` ような関数を使用することも可能です。

3. 実装の詳細: Gleamでは、文字列補間は `{}`内に変数名を含む特殊な構文を使用して行います。内部的には、これは特定の変数名に基づいて新しい文字列を生成します。

## さらに学ぶ：

- Gleam公式ドキュメント: [https://gleam.run/book/tour/introduction.html](https://gleam.run/book/tour/introduction.html)
- 文字列連結と補間: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)