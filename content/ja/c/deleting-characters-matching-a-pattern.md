---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列からパターンに一致する文字を削除するとは、特定のパターン（例えば、特定の文字や文字列）を探してそれを取り除くことを意味します。これは、不要な空白や特殊文字を削除したり、文字列を一貫した形式に整形したりするのに役立ちます。

## どのように?

ここでは、Cプログラムで文字列から文字'a'を削除する方法を示します。

```C
#include <stdio.h>
#include <string.h>

void removeChars(char *str, char garbage) {
    char *src, *dst;
    for(src = dst = str; *src != '\0'; src++) {
        *dst = *src;
        if(*dst != garbage) dst++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "abcabcabc";
    
    removeChars(str,'a');
    
    printf("%s",str);
    
    return 0;
}
```

このプログラムは「bcbbcbcbc」を出力します。この出力が示す通り、すべての'a'が取り除かれています。

## ディープダイブ

履歴について言えば、このような文字削除の操作は、以前の编程語言バージョンでも利用可能で、現在と同じく、特定の目的がありました。

代替手段としては、他の多くのプログラミング言語で同様の操作が可能です。例えばPython、Javaなどです。

このような操作は、一般的には2つのポインタを利用して実行されます。ソースポインタは読み、デスティネーションポインタは書き込みます。文字が指定したパターンと一致する場合、その文字は書き込まれずにスキップされます。しかし、一致しない場合、文字は書き込まれ、デスティネーションポインタも進行します。この方法で、最小のオーバーヘッドで操作を実行することが可能です。

## 参照

1. C library function - [memmove()](https://www.tutorialspoint.com/c_standard_library/c_function_memmove.htm) 
2. C library function - [strchr()](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)