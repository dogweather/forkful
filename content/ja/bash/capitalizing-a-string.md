---
title:                "文字列の大文字化"
html_title:           "Bash: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## やめよう
文字列の大文字化を行う理由を最大2文で説明します。

## どうやるか
`` `Bash
str = "hello world"
echo -e "大文字化前：$ str"
capitalized_str = '$str' | tr '[a-z]' '[A-Z]'
echo -e "大文字化後：$ capitalized_str"
`` `
上記のようなコマンドを使用して、文字列を大文字化することができます。上記の例では、"hello world"を"HELLO WORLD"に変換します。

## 深堀り
Bashでは、単純なコマンドでも柔軟性があります。大文字化を行うためには、`tr`コマンドを使用します。このコマンドは、指定された文字を別の文字で置換することができます。具体的には、文字列の小文字を大文字に置換することができます。

## それでは見てみましょう
**### 参考リンク：**
- [Bashの公式ドキュメント](https://www.gnu.org/software/bash/manual/bash.html)
- [trコマンドの使用方法](https://www.computerhope.com/unix/utr.htm)
- [文字列の大文字化についていくつかの例を紹介するブログ記事](https://www.thegeekstuff.com/2010/07/bash-uppercase-lowercase-and-capitalize-casing-conversion/)