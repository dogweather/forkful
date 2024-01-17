---
title:                "文字列の長さを見つける"
html_title:           "Fish Shell: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何をするのか？その理由は？

文字列の長さを見つけるとは、文字列の中の文字の数を計算することです。プログラマーは、文字列を操作する際に、その長さを知る必要があるため、この操作を行います。

## 方法：

```
Fish Shellには、'string length'という組み込み関数があります。これを使用することで、文字列の長さを簡単に計算することができます。例を見てみましょう。
```

```
set my_string "Hello"
echo (string length $my_string)
=> 5
```

## 詳しく見る

### 歴史的背景

文字列の長さを見つけるという操作は、プログラミング言語やシェルの標準の機能として提供されてきました。古くは、C言語のstrlen()関数が有名です。

### 代替手段

Fish Shell以外でも、文字列の長さを見つけることができるシェルやプログラミング言語は多数あります。例えば、Bashシェルでは、'${#string}'という構文を使うことで文字列の長さを取得できます。

### 実装の詳細

Fish Shellの'string length'関数は、内部的には文字列の長さを確認することで実装されています。詳しい実装に興味がある方は、Fish Shellのソースコードを確認してみてください。

## 関連情報

- Official Fish Shell Documentation: https://fishshell.com/docs/current/
- C言語のstrlen()関数について: https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm