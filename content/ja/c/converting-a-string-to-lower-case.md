---
title:                "「文字列を小文字に変換する」"
html_title:           "C: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

＃＃ 何となぜ？

文字列を大文字から小文字に変換することは、プログラマーがテキストを操作するときに非常に便利です。大文字と小文字を区別することで、文字列を比較したり、特定の形式に整形したりすることができます。プログラマーは通常、使用する言語に応じて、文字列を大文字から小文字に変換する方法を学びます。

＃＃ 方法：

以下のコードブロックを使用して、文字列を大文字から小文字に変換する方法を説明します。

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char str[] = "HELLO WORLD!";
    int i = 0;

    // 文字列を大文字から小文字に変換するループ
    while (str[i])
    {
        putchar(tolower(str[i]));
        i++;
    }

    return 0;
}
```

出力：hello world!

＃＃ 深堀り：

文字列を大文字から小文字に変換する方法は、古いコンピューターのプログラマーにとって重要な基本的なスキルでした。その当時、コンピューターは大文字と小文字を区別せず、すべてのコマンドは大文字で入力する必要がありました。しかし、現代では、大文字と小文字を区別することでテキストをより柔軟に操作できるようになりました。

文字列を大文字から小文字に変換する方法は、プログラミング言語ごとに異なります。C言語では、```tolower```関数を使用して、文字列を小文字に変換します。ただし、他の言語では、異なるメソッドや関数を使用する必要があるかもしれません。

＃＃ 関連情報：

- https://www.programiz.com/c-programming/library-function/ctype.h/tolower
- https://www.geeksforgeeks.org/c-program-find-lowercase-string/
- https://www.tutorialspoint.com/c_standard_library/ctype_h.htm