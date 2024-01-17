---
title:                "パターンに一致する文字の削除"
html_title:           "C: パターンに一致する文字の削除"
simple_title:         "パターンに一致する文字の削除"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何かが合いませんか？
文字列を検索し、特定のパターンにマッチする文字を削除することは、プログラマーにとってよくあるタスクです。例えば、ユーザーが入力した電話番号にあるハイフンを取り除くことが求められる場合など、様々な場面で使用されます。

## 方法：
次のコード例をご覧ください：
```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[20] = "123-456-789";
    char pattern = "-";

    for (int i = 0; i < strlen(str); i++)
    {
        if (str[i] == pattern)
        {
            memmove(&str[i], &str[i+1], strlen(str)-i);
            i--;
        }
    }

    printf("%s", str);
    return 0;
}
```
出力:
```
123456789
```

## 深く潜る：
文字を削除する方法は、プログラミング言語や環境によって異なりますが、一般的なアルゴリズムは同じです。パターンマッチングアルゴリズムを使用して、文字列内で指定されたパターンを検索し、削除します。代替手段としては、指定された文字が含まれるかどうかを確認し、その文字を削除する方法もあります。パターンマッチングアルゴリズムを実装するには、文字列を反復処理し、パターンにマッチする文字を見つけて削除する必要があります。

## 関連情報：
- [C言語の文字列操作](https://www.tohoho-web.com/ex/cstring.html)
- [文字列処理関数の概要](https://www.programming-technique.com/2019/08/c-function-text-string.html)