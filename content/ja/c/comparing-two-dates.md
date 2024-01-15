---
title:                "二つの日付を比較する"
html_title:           "C: 二つの日付を比較する"
simple_title:         "二つの日付を比較する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

二つの日付を比較することに関わる理由は、プログラミングで日付を処理する必要がある場合によくあるシナリオです。日付を比較することは、例えばアプリケーションでユーザーが特定の日付を選択した場合に特定のアクションを実行するために必要になるかもしれません。

## 方法

まず、日付を比較する前に、2つの日付を同じ形式に変換する必要があります。以下のような例を見てみましょう。

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 2つの日付を定義する
    char date1[] = "01/01/2021";
    char date2[] = "01/05/2021";

    // 2つの日付をtime構造体に変換する
    struct tm tm1 = {0};
    struct tm tm2 = {0};
    strptime(date1, "%d/%m/%Y", &tm1);
    strptime(date2, "%d/%m/%Y", &tm2);

    // 比較する
    if (mktime(&tm1) > mktime(&tm2)) {
        printf("%s is later than %s\n", date1, date2);
    } else if (mktime(&tm1) < mktime(&tm2)) {
        printf("%s is earlier than %s\n", date1, date2);
    } else {
        printf("%s is equal to %s\n", date1, date2);
    }
    return 0;
}
```

上記のコードは、`strptime()`関数を使用して文字列を日付として認識し、`mktime()`関数を使用して日付を比較します。出力は次のようになります。

```
01/01/2021 is earlier than 01/05/2021
```

## 深堀り

日付を比較する際に、timezone（タイムゾーン）やleap seconds（閏秒）など、考慮すべきことがあります。C言語の標準ライブラリには、`mktime()`と`difftime()`関数があり、これらはtimezoneやleap secondsを考慮しないため、比較結果にエラーが生じる可能性があります。

より正確な日付の比較を行うには、外部ライブラリを使用することが推奨されます。例えば、timegmライブラリはtimezoneを考慮せずに日付を比較することができます。

## 参考文献

- [C言語で日付を比較する方法](https://www.tutorialspoint.com/how-to-compare-dates-in-c-programming)
- [timezoneやleap secondを考慮するための外部ライブラリ](https://stackoverflow.com/questions/2219505/comparing-dates-in-c-without-considering-the-tz-and-daylight-saving-time-bugs)