---
title:    "C: 「未来や過去の日付の計算」"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ？

日々の生活では、未来や過去の日付を計算することが必要になることがあります。例えば、特定のイベントの日程を確認したり、予定を立てたりする際に、必要な日付を正確に計算することが重要です。そのため、C言語で日付を計算できるようになると、より効率的なプログラミングができるようになります。

## 使い方

日付の計算を行うには、C言語で提供されている標準ライブラリのtime.hを使用します。具体的なコード例を以下に示します。

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    struct tm *future_date;
    future_date = localtime(&now);

    // 10日後の日付を計算する
    future_date->tm_mday += 10;

    // 月末の場合は、月と年を調整する
    mktime(future_date);

    // 日付をフォーマットして出力する
    printf("10日後の日付は%02d/%02d/%dです", future_date->tm_mon + 1, future_date->tm_mday, future_date->tm_year + 1900);

    return 0;
}
```

上記のコードでは、time.hの構造体tmを使用して、現在の日付から10日後の日付を計算し、フォーマットして出力しています。また、月末の場合には月や年の調整も行っています。

## 深く掘り下げる

C言語では、time.h以外にも日付計算を行うためのライブラリがいくつかあります。例えば、Julian Day（ユリウス日）を使用して日付を計算する手法や、外部ライブラリを使用して暦の種類に応じた日付計算を行う方法などがあります。また、日付計算を行う際には、タイムゾーンや夏時間などの影響も考慮する必要があります。

# 参考リンク

- [C言語の標準ライブラリを使って日付を扱う方法（別冊実践C言語）](https://developers.line.biz/ja/docs/messaging-api/technical-reference/#postback-action)
- [C言語における時間管理と変換（それでも月日及び月日のためのグレゴリオ暦の場合）](https://docs.microsoft.com/ja-jp/windows/win32/sysinfo/converting-a-time-between-utc-and-local-time)