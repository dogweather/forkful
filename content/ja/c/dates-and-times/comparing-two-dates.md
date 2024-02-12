---
title:                "二つの日付を比較する"
aliases: - /ja/c/comparing-two-dates.md
date:                  2024-02-03T17:53:43.915745-07:00
model:                 gpt-4-0125-preview
simple_title:         "二つの日付を比較する"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/comparing-two-dates.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

C言語で二つの日付を比較することは、それらの日付間の時間的な関係—一方の日付が他方よりも前か、あるいは同じかを決定することを含みます。この能力は、スケジューリング、締め切り、または記録保持を扱うアプリケーションにとって重要であり、時間に敏感なデータの整理や操作を可能にします。

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
