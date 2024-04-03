---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:43.915745-07:00
description: "\u65B9\u6CD5\uFF1A C\u306B\u306F\u65E5\u4ED8\u306E\u7D44\u307F\u8FBC\
  \u307F\u578B\u304C\u306A\u3044\u305F\u3081\u3001`time.h`\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3057\u3066\u65E5\u4ED8\u3068\u6642\u523B\u306E\u69CB\u9020\
  \u4F53\u3092\u6271\u3046\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002`tm`\u69CB\
  \u9020\u4F53\u3068`difftime()`\u95A2\u6570\u304C\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\
  \u308B\u305F\u3081\u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306F\u4E8C\u3064\u306E\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\
  \u65B9\u6CD5\u3092\u793A\u3059\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.814862-06:00'
model: gpt-4-0125-preview
summary: "C\u306B\u306F\u65E5\u4ED8\u306E\u7D44\u307F\u8FBC\u307F\u578B\u304C\u306A\
  \u3044\u305F\u3081\u3001`time.h`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\
  \u3057\u3066\u65E5\u4ED8\u3068\u6642\u523B\u306E\u69CB\u9020\u4F53\u3092\u6271\u3046\
  \u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002`tm`\u69CB\u9020\u4F53\u3068`difftime()`\u95A2\
  \u6570\u304C\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u305F\u3081\u306B\u4E00\u822C\
  \u7684\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u4E8C\u3064\
  \u306E\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3059\u4F8B\
  \u3067\u3059\uFF1A."
title: "\u4E8C\u3064\u306E\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## 方法：
Cには日付の組み込み型がないため、`time.h`ライブラリを使用して日付と時刻の構造体を扱う必要があります。`tm`構造体と`difftime()`関数が日付を比較するために一般的に使用されます。以下は二つの日付を比較する方法を示す例です：

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // 最初の日付 (YYYY, MM, DD)
    date1.tm_year = 2023 - 1900; // 1900年からの年
    date1.tm_mon = 3 - 1;        // 月 [0-11]
    date1.tm_mday = 15;          // 月の日にち [1-31]

    // 2番目の日付 (YYYY, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // time_t形式に変換
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // 比較
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("日付は同じです。\n");
    } else if (seconds > 0) {
        printf("最初の日付は2番目の日付より後です。\n");
    } else {
        printf("最初の日付は2番目の日付より前です。\n");
    }

    return 0;
}
```

出力は次のようになる可能性があります：

```text
最初の日付は2番目の日付より前です。
```

このプログラムは2つの`tm`構造体を特定の日付で初期化し、`mktime()`を使用して`time_t`形式に変換し、最後に`difftime()`を使って比較します。これは2つの時間の違いを秒で返します（`double`型で）。

## 深堀り
Cの初期には、日付と時刻の操作には手動の計算が必要であり、うるう年、月の日数の変動、さらにはうるう秒を考慮することがよくありました。ANSI C標準で`time.h`の導入により、Cの時間処理に標準化がもたらされ、日付と時刻の操作が簡素化されました。

`time.h`を使用した日付比較は直接的ですが、制限があります。`tm`構造体はタイムゾーンや夏時間を考慮しておらず、`difftime()`は秒単位の差異のみを提供し、特定のアプリケーションには精度が不足しています。

タイムゾーン、夏時間の切り替え、より正確な時間間隔を含む、より堅牢な日付・時刻操作を求めるアプリケーションでは、`date.h`（標準ライブラリーの一部ではないHoward Hinnant日付ライブラリー）のようなライブラリーが`time.h`の現代的な代替手段を提供します。これらのライブラリーは、数十年にわたるプログラミング言語設計の進化を受けて、C++での日付・時刻操作のためのより包括的なツールを提供します。Cプログラマーにとっては、これらの外部ライブラリーを利用するか、日付・時刻の計算の複雑さを直接丁寧に扱うことが、正確で文化的に敏感な日付・時刻操作を実現するために必要とされます。
