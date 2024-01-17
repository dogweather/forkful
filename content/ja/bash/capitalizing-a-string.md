---
title:                "文字列の先頭を大文字にする"
html_title:           "Bash: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何？その理由は？

文字列を大文字化するとは何か？プログラマーがそれをやる理由は？

文字列を大文字化することとは、文字列をすべて大文字に変換することです。プログラマーたちがそれを行う理由は、大文字と小文字を区別しなければならない場合や、入力が予期しない形式で与えられた時に、正しい処理を行うためです。

## 方法：

```Bash
# 大文字化する方法は２つあります

# 1. trコマンドを使用する方法
echo "hello world" | tr '[:lower:]' '[:upper:]'

# 出力: HELLO WORLD

# 2. bashのパラメータ展開を使用する方法
text="hello world"
echo "${text^^}"

# 出力: HELLO WORLD
```

## 詳しく見ていく：

- **歴史的背景：** 文字列の大文字化は、古くからプログラミング言語やコンピュータシステムで用いられてきました。これは主に、大文字小文字の区別が重要なプログラムでの処理を行うために必要だったからです。

- **代替手段：** もちろん、大文字化するための他の方法もあります。例えば、文字列の一部を大文字化する必要がある場合は、sedコマンドを使ったり、文字列置換を行うこともできます。ただし、bashのパラメータ展開やtrコマンドが比較的簡単で直感的な方法です。

- **実装の詳細：** 我々が使用したtrコマンドは、文字列に見つかった小文字を大文字に変換するよう設定されています。また、bashのパラメータ展開は、文字列の変数の最初の文字を大文字にすることで全体を大文字化しています。

## さらに見る：

- [The tr command](https://linux.die.net/man/1/tr)
- [Parameter Expansion in Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Uppercase a string in Bash](https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash)