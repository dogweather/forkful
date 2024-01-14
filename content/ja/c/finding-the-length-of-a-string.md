---
title:                "C: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることの意義は何でしょうか？プログラマーにとって、文字列の長さを知ることは非常に重要です。そこで、今回はC言語で文字列の長さを求める方法を解説します。

## 方法

文字列の長さを求めるには、`strlen()`という関数を使用します。これはC言語に標準で用意されている関数で、文字列の長さを返してくれます。以下に例を示します。

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "こんにちは、世界！";
    int length = strlen(str);
    printf("文字列の長さは%dです。\n", length);
    return 0;
}
```

このコードを実行すると、コンソールには「文字列の長さは13です。」と表示されます。このように、`strlen()`関数を使用すれば、簡単に文字列の長さを求めることができます。

## 詳しく見ていこう

前述のコードでは、`strlen()`関数を使用していますが、実際にはどのような処理が行われているのでしょうか？`strlen()`関数は、与えられた文字列の末尾に`\0`という文字が付加されているかぎり、その文字列の先頭から何バイト分のメモリを使用するかを数えることで長さを求めます。これは`\0`が文字列の終端を示す特別な文字であることを利用しています。また、`strlen()`関数は「Unicodeによるマルチバイト文字列」にも対応しているので、非ASCII文字を含む文字列でも正確に長さを求めることができます。

## 参考になる情報

- [C言語リファレンス - strlen](https://ja.wikipedia.org/wiki/Strlen)
- [ASCIIとUnicode（UTF-8）の違いを理解する](https://w3g.jp/blog/tips/ascii_and_unicode)