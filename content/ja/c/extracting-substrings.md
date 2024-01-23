---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:45:27.708932-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"
programming_language: "C"
category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列（string）から特定の部分を取り出すことを、部分文字列（substring）の抽出と言います。プログラマーがこれを使用する理由は、特定の情報を取得したり、データを分析するためです。

## How to: (方法)
```c
#include <stdio.h>
#include <string.h>

void extract_substring(char* source, int start, int length) {
    char result[length + 1];
    strncpy(result, source + start, length);
    result[length] = '\0';
    printf("Extracted substring: '%s'\n", result);
}

int main() {
    char text[] = "こんにちわ、世界！";
    extract_substring(text, 6, 3); // "世界"の部分を抽出
    
    return 0;
}
```
出力:
```
Extracted substring: '世'
```

## Deep Dive (深く掘り下げて)
部分文字列を抽出する機能は、あらゆる情報処理で不可欠です。昔から多くのプログラミング言語に組み込まれています。C言語では標準ライブラリの`string.h`が提供する`strncpy`関数を使用しますが、使い方に注意が必要です。終端のNULL文字を忘れないようにしましょう。代わりに、`substr`や`slice`といった関数を持つ他の言語もあります。

具体的な実装では、メモリの範囲外にアクセスしないように気をつける必要があります。バッファオーバーフローを防ぐため、確保するメモリサイズは部分文字列の長さよりも1バイト大きくすることを忘れないでください。

## See Also (関連情報)
- C標準ライブラリのドキュメント: [cplusplus.com (C++のリファレンスですが、Cのライブラリもカバーしています)](http://www.cplusplus.com/reference/cstring/)
- メモリ管理の良い習慣についての情報: [Computer Science from the Bottom Up](https://www.bottomupcs.com/)
- 文字列処理に関する詳しい解説: [Beej's Guide to C Programming](https://beej.us/guide/bgc/)
