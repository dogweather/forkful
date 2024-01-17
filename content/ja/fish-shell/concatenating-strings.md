---
title:                "文字列の結合"
html_title:           "Fish Shell: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

##なに? & なぜ?

文字列を連結することは、プログラマーがよく行う作業の一つです。文字列を連結すると、一つの長い文字列にすることができます。これにより、より使いやすいコードを書くことができます。

## 使い方:

```Fish Shell```のコードブロック内で、文字列を連結する方法を示します。

```fish
set str1 "Hello"
set str2 "World"
echo $str1$str2
```

上記のコードを実行すると、```HelloWorld```という文字列が出力されます。

## 詳細を掘り下げる:

文字列を連結する方法は、プログラミング言語によって異なります。Fish Shellでは、```echo```コマンドの後に```$```記号を付けることで、文字列を連結できます。他のプログラミング言語でも、同様の構文を使用することで文字列を連結できることが多いです。

他にも、文字列を連結する方法としては、```concat()```関数を使用する方法や、```+```演算子を使用する方法などがあります。

## 参考リンク:

- [Concatenation (programming)](https://en.wikipedia.org/wiki/Concatenation_(programming))
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [Programming with Strings in Fish Shell](https://medium.com/@xirone011/programming-with-strings-in-fish-shell-552d6b6e8cf)