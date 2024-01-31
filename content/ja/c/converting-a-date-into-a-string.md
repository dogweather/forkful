---
title:                "日付を文字列に変換する"
date:                  2024-01-20T17:36:19.194780-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

category:             "C"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するっていうのは、プログラム内での日付データを人間が読める形式にすることです。なぜやるかって？データの表示、記録、通信の簡単化のためです。

## How to: (やり方)
```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    struct tm *tm_now = localtime(&now);
    char date_str[100];

    strftime(date_str, sizeof(date_str), "%Y-%m-%d %H:%M:%S", tm_now);
    printf("現在の日付と時刻: %s\n", date_str);
    
    return 0;
}
```
出力例:
```
現在の日付と時刻: 2023-04-05 21:30:52
```

## Deep Dive (掘り下げ)
C言語での日付の扱いは、1970年代のUNIX時代からあります。`time.h`ヘッダーはこれをサポートし、`time()` 関数を使って現在時刻を取得し、`localtime()` を使って地域ごとにカスタマイズできる形に変換します。`strftime()` 関数は、日付と時刻を様々な形式の文字列に変換できます。

代替手段の1つとして、`snprintf()` などの一般的な文字列フォーマット関数を使うこともできますが、日付や時刻に特化していないため、使いにくいかもしれません。`strftime()` はカスタマイズ可能で便利ですが、完全に振る舞いを理解するのには`time.h`のドキュメントをしっかり読む必要があります。

## See Also (関連情報)
- C標準ライブラリの`time.h`: https://en.cppreference.com/w/c/chrono
- `strftime()`のフォーマット指定子: https://en.cppreference.com/w/c/chrono/strftime
- POSIXの日付と時刻のAPI: https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html
