---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の長さを見つけるとは、文字列が何文字含まれているかを数えることです。プログラマーがこれを行う理由は、文字列操作、バリデーション、配列のサイズを制御するなど、多岐にわたります。

## どうやって：

```C
#include <stdio.h>
#include <string.h>

int main() {
   char str1[30] = "Hello, World!";
   int length;

   length = strlen(str1);

   printf("Length of |%s| is |%d|\n", str1, length);

   return (0);
}
```

このプログラムを実行した結果は次のとおりです：

``` 
Length of |Hello, World!| is |13|
```

## ディープダイブ

歴史的な文脈：初期のコンピュータプログラミング言語では、文字列の長さを自動的に計算する機能はありませんでした。そのため、文字列の終わりを示す特別な記号を使うか、プログラマ自身が長さを追跡しなければなりませんでした。これは、C言語で採用された'\0'（ヌル文字）の起源です。 

代替方法： `strlen` は非常に一般的であり、ほとんどの場合で有効ですが、場合によっては他の方法がより適していることもあります。例えば、`strnlen` 関数は文字列の最大長さを指定できるため、安全なコードを書く際に有用です。

実装の詳細： `strlen` の動作は簡単です。指定したポインタから始まり、ヌル文字に到達するまでメモリ上を逐次走査します。その結果、返される値はヌル文字を除く文字数です。

## 参考資料

1. `strlen` function: https://www.cplusplus.com/reference/cstring/strlen/
2. `strnlen` function: https://en.cppreference.com/w/c/string/byte/strnlen
3. Null-terminated strings: https://www.learncpp.com/cpp-tutorial/66-c-strings/