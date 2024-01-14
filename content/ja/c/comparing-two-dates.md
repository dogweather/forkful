---
title:                "C: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較するのに、なぜ 人々が関わるのかを説明するための 1 〜 2 文があります。

日付を比較する必要があるいくつかの理由があります。例えば、二つのイベントが同じ日付や同じ期間に行われるかどうかを確認したり、期限を設定するために日付を比較することができます。また、日付を比較することで、データベース内の特定の日付に関連する情報を検索することもできます。

## 方法

日付を比較するには、いくつかの方法がありますが、ここではC言語を使用した例を紹介します。C言語は、日付や時間を扱う機能を提供しているため、日付を比較するには最適な言語です。

**例 1:** 二つの日付を比較するプログラム

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 比較する日付を設定
    struct tm date1 = { .tm_year = 2020, .tm_mon = 0, .tm_mday = 1 };
    struct tm date2 = { .tm_year = 2020, .tm_mon = 0, .tm_mday = 15 };

    // 時間を0時0分0秒に設定
    date1.tm_hour = 0;
    date1.tm_min = 0;
    date1.tm_sec = 0;

    date2.tm_hour = 0;
    date2.tm_min = 0;
    date2.tm_sec = 0;

    // 日付を秒数に変換して比較
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // 日付を比較
    if (time1 < time2) {
        printf("date1はdate2より前の日付です。");
    } else if (time1 == time2) {
        printf("date1とdate2は同じ日付です。");
    } else {
        printf("date1はdate2より後ろの日付です。");
    }

    return 0;
}
```

**出力:**

```
date1とdate2は同じ日付です。
```

**例 2:** 特定の日付を検索するプログラム

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 検索する日付を設定
    struct tm search_date = { .tm_year = 2020, .tm_mon = 0, .tm_mday = 31 };

    // 時間を0時0分0秒に設定
    search_date.tm_hour = 0;
    search_date.tm_min = 0;
    search_date.tm_sec = 0;

    // 日付を秒数に変換
    time_t search_time = mktime(&search_date);

    // 検索結果を表示する日付を設定
    struct tm result_date = { .tm_year = 0, .tm_mon = 0, .tm_mday = 0 };

    // データベースなどから日付を取得
    // ここでは空の例を用いる
    int data1 = 2020;
    int data2 = 1;
    int data3 = 31;

    // 日付を結果に反映
    result_date.tm_year = data1 - 1900;
    result_date.tm_mon = data2 - 1;
    result_date.tm_mday = data3;

    // 日付を秒数に変換
    time_t result_time = mktime(&result_date);

    // 検索結果を比較
    if (search_time == result_time) {
        printf("日付が一致しました。");
    } else {
        printf("日付が一致しませんでした。