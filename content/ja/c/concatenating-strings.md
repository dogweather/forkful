---
title:                "文字列の連結"
html_title:           "C: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となんで?
文字列を連結するとは何かを説明し、プログラマーがなぜそれを行うかを説明する。

## 方法:
```C
#include <stdio.h>

int main() {
   char str1[20] = "Hello";
   char str2[20] = "World";
   strcat(str1, str2);
   printf("Concatenated string: %s", str1);
   return 0;
}
```

出力: Concatenated string: HelloWorld

## 詳しく見る:
文字列を連結するとは、2つの文字列を1つの文字列に結合することを指します。プログラマーは、複数の文字列を結合して新しい文字列を作成することで、より複雑なプログラムを開発することができます。

歴史的な文脈では、文字列を連結するために使用されていた最も古い関数は、C言語のstrcat()関数でした。ただし、その後も様々な関数や方法が開発されました。

文字列を連結するための代替手段として、文字列の作成時に直接連結する方法や、フォーマット指定子を使用して連結する方法などがあります。

文字列を連結する方法の実装詳細については、コンピュータサイエンスの概念である「文字列」や「メモリ」などについて学ぶ必要があります。

## 関連をチェック:
- [C言語の文字列分割チュートリアル](https://www.learn-c.org/en/String_Splitting)
- [文字列操作用のC言語ライブラリ関数](https://www.programiz.com/c-programming/library-function/string.h)
- [C言語文字列のフォーマット](https://www.codingunit.com/c-tutorial-formatting-strings)