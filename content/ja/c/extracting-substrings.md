---
title:                "C: 部分列を抽出する"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# なぜサブストリングを抽出するのか

サブストリングを抽出するとは、文字列の一部分を取り出すことです。これには多くの理由があります。例えば、大きな文字列から必要な情報だけを取り出したい場合や、文字列を特定のパターンで分割したい場合などです。

## 抽出方法

C言語では、サブストリングを抽出するための便利な関数が用意されています。例えば、```strncat()```を使うことで、任意の長さのサブストリングを取得することができます。以下にサンプルコードを示します。

```
#include <stdio.h>
#include <string.h>

int main() {
  char str1[50] = "Hello";
  char str2[50] = "World";
  char result[50];

  // strncatを使用して、str1から最初の3文字を抽出
  strncat(result, str1, 3);
  printf("Result: %s\n", result);

  // 文字列の連結を行い、resultに格納
  strncat(result, str2, 3);
  printf("Result: %s\n", result);

  return 0;
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
Result: Hel
Result: HelWor
```

このように、```strncat()```を使用することで、必要なサブストリングを取得することができます。

## 詳しい解説

サブストリングを抽出する方法は、文字列処理において非常に重要です。C言語では、文字列操作に関する様々な関数が提供されていますが、サブストリングを抽出するためには特に注意が必要です。文字列の長さや文字の種類によって、適切な関数を選択する必要があります。

また、サブストリングを抽出する際には、メモリ管理にも注意が必要です。サブストリングを取得するためには、必要な長さだけのメモリを確保する必要があります。その際には、配列の長さと確保するメモリの長さが一致するようにすることが重要です。

# 参考リンク

- [C言語でのサブストリングの抽出方法](https://www.studytonight.com/c/substring-manipulation-in-c.php)
- [String Functions in C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Understanding String Manipulation in C](https://www.techiedelight.com/string-manipulation-in-c/)
- [C言語入門をしよう](https://www.code4startup.com/article/c-for-beginners)