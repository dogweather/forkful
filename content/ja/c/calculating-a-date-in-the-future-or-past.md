---
title:                "未来または過去の日付を計算する"
html_title:           "C: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何と何のために?

日付の計算とは、現在の日付から特定の日数を加算または減算して将来または過去の日付を得ることを指します。プログラマーがこれを計算する理由は、予定の日程管理、期限の追跡、イベントのスケジューリングなど、多岐にわたります。

## 使い方:

C言語を使用して未来または過去の日付を計算するには、以下のようなコードを使用します：

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 現在の時間を取得
    time_t now;
    time(&now);

    // 5日後の日付を計算
    struct tm *five_days_later = localtime(&now);
    five_days_later->tm_mday += 5;
    mktime(five_days_later);

    printf("5 days later: %s", asctime(five_days_later));

    return 0;
}
```

このコードの出力は次のようになります:

```
5 days later: Wed Jan 27 13:30:15 2021
```

## ディープダイブ:

日付操作はプログラミングの歴史の初期から存在し、様々な方法で実装されてきました。C言語では`time.h`ライブラリを使用することで、時間に関する多くの操作が可能です。

ただし、単純に日を加算・減算するだけでは、時間単位の異なる月と年に対しては適切に機能しません。このような場合、特定の関数を使用して適切な日付を取得することが推奨されます。

また、このプロセスの代わりに、多くの開発者が既存の日付・時間ライブラリを活用しています。これらは、日付間の差分計算、時刻の変換、時間帯の管理など、より複雑な日付操作を可能にします。

## 関連情報:

以下のリンクから関連内容を詳しく学ぶことができます:

- [C date and time programming](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C programming examples of date and time](https://www.javatpoint.com/date-and-time-in-c)