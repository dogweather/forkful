---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "C"
category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なに？ どうして？)

文字列の大文字化とは、小文字を全部大文字に変える処理です。それは見た目の統一や、入力データの標準化のために使います。

## How to: (やり方)

C言語で文字列を大文字にする例を見てみましょう。

```c
#include <stdio.h>
#include <ctype.h>

void capitalize(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "こんにちは、プログラマー!";
    
    capitalize(text);
    printf("大文字化: %s\n", text);
    
    return 0;
}
```
サンプル出力:
```
大文字化: こんにちは、プログラマー!
```

注意：上記のコードはASCII文字にのみ適用されます。日本語の文字など、非ASCIIは大文字化されません。

## Deep Dive (掘り下げ)

文字列の大文字化は、C言語が始まった1970年代初頭から利用されています。`toupper`関数は標準ライブラリに含まれ、文字ごとに大文字への変換を行います。

代替手段として、`<locale.h>`ヘッダを使い、ロケールに基づいた大文字化が可能です。しかし、この方法は設定が少々複雑です。

実装の詳細として、`toupper`関数は引数として与えられた文字が小文字の場合にだけ、対応する大文字を返します。ASCIIテーブルに基づいて変換が行われるので、他の文字セットでは別の方法が必要になります。

## See Also (関連する情報源)

- C標準ライブラリ - `ctype.h`: http://www.cplusplus.com/reference/cctype/
- さらなる文字列操作については、`<string.h>` を参照してください: http://www.cplusplus.com/reference/cstring/
- ロケールに基づいた操作の例 (`setlocale`関数を使う): https://en.cppreference.com/w/c/locale/setlocale
