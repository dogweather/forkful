---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:38:01.600216-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、アルファベットの大文字を対応する小文字に変えることです。これは、大文字・小文字を区別せずにデータを比較したり検索したりする時に役立ちます。

## How to: (方法)
```C
#include <stdio.h>
#include <ctype.h>

void to_lowercase(char *str) {
    while (*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "Hello, C Programmer!";
    to_lowercase(text);
    printf("%s\n", text);
    return 0;
}
```
出力:
```
hello, c programmer!
```

## Deep Dive (詳細情報)
歴史的に、文字列を小文字に変換する必要性は、コンピュータが区別して扱っていたためです。ASCIIでは、大文字と小文字は異なる値を持ちます。`tolower`関数はC言語の標準ライブラリにあり、これが最も一般的な方法です。代替方法としては、ASCII値を直接操作するか、自分で変換テーブルを作成することが考えられますが、`tolower`関数を使うことでロケールに依存する問題を避け、移植性が高くなります。

## See Also (関連情報)
- `tolower` 関数: https://en.cppreference.com/w/c/string/byte/tolower
- C 標準ライブラリ: https://en.cppreference.com/w/c/header
- ASCII 表: https://www.asciitable.com/