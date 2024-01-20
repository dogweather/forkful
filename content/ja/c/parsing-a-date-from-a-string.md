---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:34:54.844894-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デート文字列の解析は、文字列から日付データへの変換です。データ処理、有効性チェック、ユーザーインターフェイスとのやり取りのためにプログラマーはこれを行います。

## How to: (方法)
```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    const char *dateString = "2023-04-05";
    struct tm tm;
    char buf[255];

    if (strptime(dateString, "%Y-%m-%d", &tm) == NULL) {
        printf("Date parsing failed.\n");
        return 1;
    } else {
        // successfully parsed, now you can use tm to manipulate date
        strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);
        printf("Parsed date: %s\n", buf);
    }

    return 0;
}
```
Sample Output:
```
Parsed date: Wednesday, April 05, 2023
```

## Deep Dive (深く掘り下げる)
日付の解析は C 標準ライブラリが 'strptime()' 関数でサポートしていますが、これは UNIX 系システムでのみ利用できます。Windows では別の関数を使うことになります。代替として、自作の解析関数を書くことも可能ですが、複雑性やエラーチェックが必要です。この関数を使うと、便利な操作ができるようになる 'struct tm' に日付が格納されます。カスタム形式で出力したい場合は 'strftime()' を使います。

## See Also (さらに参照)
- C Standard Library Documentation: https://en.cppreference.com/w/c/chrono
- GNU C Library manual for strptime: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html
- strftime format options: https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime