---
title:                "文字列を連結する"
html_title:           "C: 文字列を連結する"
simple_title:         "文字列を連結する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
なぜ文字列を連結する必要があるのでしょうか？

文字列を連結することで、複数の文字列を一つの大きな文字列に統合することができます。これにより、より効率的にデータを扱うことができるだけでなく、プログラムの実装もより簡単になります。

## How To
文字列を連結するには、C言語の`strcat()`関数を使用します。以下のように使用することができます。

```C
#include <stdio.h>
#include <string.h>

int main() {

    char first[] = "Hello";
    char second[] = "World";

    // ファーストとセカンドを連結する
    strcat(first, second);

    printf("Concatenated string: %s\n", first);

    return 0;
}
```

上記のコードでは、`strcat()`関数を使用して`first`と`second`の2つの文字列を連結しています。出力結果は以下のようになります。

```
Concatenated string: HelloWorld
```

## Deep Dive
`strcat()`関数は、C言語の`string.h`ライブラリに含まれています。この関数は、終端が`null`であるまで引数に渡された2つの文字列を連結します。ただし、最終的な連結結果がオーバーフローを引き起こす場合は、予期しない結果が生じる恐れがあることに注意してください。

また、C言語には他にも`strncat()`という関数もあります。これは、`strcat()`と同様に文字列を連結しますが、引数に指定した長さのみを連結することができます。

## See Also
より詳細な情報については、以下のリンクを参照してください。

- [C言語の`string.h`ライブラリ](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [`strcat()`のマニュアルページ](https://www.man7.org/linux/man-pages/man3/strcat.3.html)
- [`strncat()`のマニュアルページ](https://www.man7.org/linux/man-pages/man3/strncat.3.html)