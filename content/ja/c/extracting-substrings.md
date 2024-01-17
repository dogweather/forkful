---
title:                "文字列の抽出"
html_title:           "C: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となく、なぜ？

Substringとは文字列の一部分を取り出すことです。プログラマーがこの機能を使う理由は、大きな文字列から特定の部分を集めるためです。例えば、ユーザーの名前や住所を別々のフィールドに代入したり、特定の文字列のうち必要な部分だけを取り出し、それ以外を無視したりすることができます。

## 試し方：

簡単な例として、文字列の先頭から10文字を抜き出してみましょう。まず、`string`という文字列を用意します。次に、`string`の先頭を指すポインターを宣言し、`substring`という配列に10文字分のメモリを割り当てます。そして、`memcpy()`を使って`string`から`substring`に10文字分のメモリをコピーします。最後に、`substring`を出力します。

```C
#include<stdio.h>
#include<string.h>

int main() {
    char string[] = "This is a sample string.";
    char *pointer = string;
    char substring[11]; // 10 letters + 1 null terminator
    memcpy(substring, pointer, 10);
    substring[10] = '\0';
    printf("%s", substring);
    return 0;
}

// Output: This is a 
```

## 深く掘り下げる：

C言語には、`memcpy()`以外にも文字列を抜き出すための関数があります。例えば`strncpy()`や`strndup()`などがあります。また、ポインターを使ったり、文字列の長さを把握することで自分でサブストリングを作り出すことも可能です。しかし、文字列の長さが可変の場合には少し手間がかかります。

## 関連情報：

- [C言語でサブストリングを抜き出す方法](https://www.geeksforgeeks.org/c-program-extracting-substring-from-substring/)
- [Cで文字列を扱う方法](https://www.javatpoint.com/c-strings)
- [C言語のポインターについて知る](https://www.programiz.com/c-programming/c-pointers)