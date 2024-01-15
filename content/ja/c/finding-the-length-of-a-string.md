---
title:                "文字列の長さを見つける"
html_title:           "C: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを見つけることの意義は何でしょう？それは、プログラムで文字列を操作する際に必要な情報であり、文字列の処理をより効率的にするために重要なスキルです。

## 使い方

文字列の長さを見つけるには、C言語の ```strlen()```関数を使用します。この関数は、文字列を引数として受け取り、その長さを返します。例えば、次のように記述します。

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "こんにちは！";
    int length = strlen(str);
    printf("文字列の長さは %d です。\n", length);
    return 0;
}
```

上記のコードを実行すると、**文字列の長さは 6 です。**という結果が得られます。

## 詳細

C言語では、文字列はヌル終端文字（\0）で終わる文字の配列として扱われます。したがって、```strlen()```関数は、ヌル終端文字までの文字数を数えることによって、文字列の長さを見つけます。

また、この関数はマルチバイト文字にも対応しており、文字列中に日本語などのマルチバイト文字が含まれていても正しく長さを計算します。

## See Also
- [Introduction to Strings in C](https://www.geeksforgeeks.org/strings-in-c-2/)
- [C Library Functions - strlen()](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [C言語の文字列関数まとめ](https://qiita.com/tosite0345/items/ed58fcd9a38838709002)