---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:13:08.718351-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何とは？それはなぜ必要？)
コードに現在の日付を取得するのは、ログ、レポート、タイムスタンプに日付を付けたい時のこと。なぜプログラマーがそれをするかというと、情報を時系列で追跡し管理するためだ。

## How to: (方法)
```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm *now_tm;
    int year, month, day;

    now = time(NULL);
    now_tm = localtime(&now);
    year = now_tm->tm_year + 1900; // tm_year is years since 1900
    month = now_tm->tm_mon + 1;    // tm_mon is months since January (0-11)
    day = now_tm->tm_mday;         // tm_mday is day of the month (1-31)

    printf("Today's Date: %d-%02d-%02d\n", year, month, day);
    return 0;
}
```
出力例:
```
Today's Date: 2023-03-15
```

## Deep Dive (深堀り)
### 歴史的背景
`time.h`ヘッダーはC標準ライブラリの一部で、時間関連の関数を含んでいる。`time()`関数は1970年1月1日からの秒数を返す。これを`localtime()`に渡すと、ローカル時間に変換される。

### 代替案
`gettimeofday()`はもっと正確な時間（マイクロ秒単位）を提供する。ただし、`time.h`だけでなく`sys/time.h`もインクルードする必要がある。

### 実装詳細
`time_t`は時刻を表すデータ型であり、`struct tm`は時刻を分解した構造体。`tm_year`は1900年からの年数を、`tm_mon`は0から始まる月数を、`tm_mday`はその月の日付をそれぞれ表す。

## See Also (参照)
- C Standard Library: https://en.cppreference.com/w/c/header
- `localtime()` function: https://en.cppreference.com/w/c/chrono/localtime
- `time()` function: https://en.cppreference.com/w/c/chrono/time
