---
title:                "C: 文字列の長さを見つける"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを探すことに関わる理由は、多くのプログラマーにとって日常的な作業です。文字列の長さを知ることで、プログラムの中で文字列を処理する際に必要な情報を得ることができます。例えば、文字列をコピーする際にはコピー先の配列のサイズを決めるために文字列の長さが必要になります。そのため、プログラミングをする上で、文字列の長さを知ることは非常に重要です。

## 方法

文字列の長さを得るには、文字列の最後にNULL文字が入っていることを前提として、文字列の最初から最後までを順番に数えていく方法が一般的です。以下にC言語での例を示します。

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "こんにちは、世界！";
    int length = 0;

    while (str[length] != '\0')
    {
        length++;
    }

    printf("文字列の長さは%dです。\n", length);

    return 0;
}
```

このコードを実行すると、以下のように出力されます。

```
文字列の長さは8です。
```

この方法で文字列の長さを得ることができます。

## ディープダイブ

C言語では、文字列の長さを得るために便利な関数が用意されています。`strlen`関数がその一つです。先ほどのコードを`strlen`関数を使って書き換えると、以下のようになります。

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "こんにちは、世界！";
    int length = strlen(str);

    printf("文字列の長さは%dです。\n", length);

    return 0;
}
```

出力結果は先ほどと同じですが、コードの行数が少なくなり、よりスマートになります。また、`strlen`関数は任意のポインタを受け取るため、動的に確保された文字列に対しても使用することができます。

## See Also

- [C言語リファレンス - 文字列の長さを得る](http://www.c-tipsref.com/reference/strlen.html)
- [C言語のミニマムリファレンス - 文字列の長さを取得する](https://zeddios.tistory.com/64)
- [Programming in C - Finding the length of a string](https://www.programiz.com/c-programming/examples/string-length)

以上でC言語における文字列の長さの取得方法についてご紹介しました。プログラミングをする際には、文字列の長さを知ることで効率的に文字列を処理することができますので、ぜひ覚えておいてください。