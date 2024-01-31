---
title:                "テキストの検索と置換"
date:                  2024-01-20T17:57:24.715205-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストを検索して置換するのは、文字列の中で特定の部分を見つけ出し、それを別のテキストに変えるプロセスです。プログラマーは、コードやデータを修正、最適化、または更新するためにこれをよく行います。

## How to: (方法)
```C
#include <stdio.h>
#include <string.h>

void replaceSubstring(char *input, const char *search, const char *replace) {
    char buffer[1024];
    char *p;

    if(!(p = strstr(input, search))) { // 文字列が見つからない
        printf("%s\n", input);
        return;
    }
    
    strncpy(buffer, input, p - input); // 置換前の部分をコピー
    buffer[p - input] = '\0';
    
    sprintf(buffer + (p - input), "%s%s", replace, p + strlen(search)); // 置換後のテキストを追加
    strcpy(input, buffer); // 元の入力に結果をコピー

    printf("%s\n", input); // 結果表示
}

int main() {
    char text[] = "Hello, World! Programming in C is fun.";
    printf("Original text: %s\n", text);
    replaceSubstring(text, "World", "Universe");
    printf("Text after replacement: %s\n", text);
    return 0;
}
```
実行結果:
```
Original text: Hello, World! Programming in C is fun.
Text after replacement: Hello, Universe! Programming in C is fun.
```

## Deep Dive (深い潜水)
文字列の置換はコンピュータサイエンスにおいて基本的な操作です。1960年代からあり、エディタ、ワープロ、データベース、開発ツールなどで使用されてきました。`strstr()`、`strncpy()`、そして `sprintf()` 関数などがC言語においてテキスト操作に使用されますが、他の方法（例えば正規表現）でも可能です。しかし、ポインタ操作に注意が必要です。間違えると、メモリ漏れやバッファオーバーフローを引き起こす可能性があります。

## See Also (参照)
- C Standard Library: https://en.cppreference.com/w/c/string/byte
- Online C Compiler: https://www.onlinegdb.com/online_c_compiler
- Regular expressions in C (POSIX library): https://www.regular-expressions.info/posix.html
