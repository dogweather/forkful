---
title:                "文字列の連結"
date:                  2024-01-20T17:34:19.710293-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？そしてどうして？)
文字列の連結とは、複数の文字列をつなげて一つの文字列にすることです。プログラマーは、メッセージを組み立てたり、データを整形するためにこれを行います。

## How to: (やり方)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Hello, ";
    char str2[] = "World!";

    strcat(str1, str2); // str1の末尾にstr2を連結する。
    printf("%s\n", str1); // 出力: Hello, World!
    
    return 0;
}
```
このコードは、`strcat` 関数を使って `str1` と `str2` を結合し、"Hello, World!"を画面に表示します。

## Deep Dive (掘り下げ)
文字列の連結は古くからある操作で、Cの標準ライブラリである `<string.h>` には `strcat` 関数が備わっています。しかし、`strcat` 関数はオーバーフローのリスクを伴うため、代替として `strncat` や他の安全な関数の使用が推奨されます。連結プロセスの詳細には、文字列の長さをチェックし、メモリを適切に確保する必要があります。Cでは、文字列操作はプログラマーの責任範囲に大きく依存しています。

## See Also (関連情報)
- C Standard Library - `strcat`: [cppreference.com/w/c/string/byte/strcat](https://en.cppreference.com/w/c/string/byte/strcat)
- C String Handling Library - `strncat`: [cppreference.com/w/c/string/byte/strncat](https://en.cppreference.com/w/c/string/byte/strncat)
- Secure Coding in C: [www.cert.org/secure-coding/](https://www.cert.org/secure-coding/)
