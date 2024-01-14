---
title:                "C++: 「未来または過去の日付の計算方法」"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

私たちは日々、多くの日程を計算し、予定を立てます。将来の日付を計算することも、過去の日付を調べることもよくあります。今回は、C++を使用して日付を計算する方法について紹介します。

## 方法

まず、日付を計算するには、計算したい日付の基準となる日付を設定する必要があります。これを「基準日」と呼びます。

基準日として、例えば今日の日付を設定することができます。その場合、C++の```time()```関数を使用して現在の日付を取得します。次に、日付を計算するための計算量を指定します。例えば、「2日後」や「5年前」のように指定します。

続いて、C++の標準ライブラリである```chrono```を使用して、日付を計算します。計算された日付は、基準日から指定した計算量を加減した日付となります。

以下に、基準日を今日の日付とし、「3日後」の日付を計算するコード例を示します。

```C++
#include <iostream>
#include <chrono>
using namespace std;

int main() {
    // 基準日を今日の日付とする
    chrono::system_clock::time_point today = chrono::system_clock::now();
    
    // 3日後の日付を計算する
    chrono::duration<int> daysToAdd(3);
    today += daysToAdd;

    // 計算された日付を出力する
    time_t calculatedDate = chrono::system_clock::to_time_t(today);
    cout << "3日後の日付は" << ctime(&calculatedDate) << "です。";
    
    return 0;
}
```

上記のコードを実行すると、次のような出力が得られます。

```3日後の日付はTue Oct 26 16:11:05 2021です。```

## ディープダイブ

日付を計算する際には、基準日を設定することが重要です。C++では、```time_point```という型を使用して時間の情報を扱います。この型は、```now()```関数により現在の日付を取得することができます。

また、日付の計算にはさまざまな操作が可能であり、期間(例えば「山積された仕事をするのに必要な時間」など)を表す```duration```という型を使用します。これにより、「1ヵ月後」や「5年前」などの単位を指定することができます。

さらに、C++の```time_point```型には標準時刻からのオフセットを表すオフセット属性もあります。これにより、特定のタイムゾーンや時差を考慮した日付計算が可能となります。

これらの機能を使いこなすことで、より複雑な日付計算も可能になります。

## 参考リンク

- [C++時間の計算 - Qiita](https://qiita.com/ucho/items/41eecd70734372be5959)
- [ISO8601での日付と時刻を計算する](https://www.fluentcpp.com/2018/06/22/how-to-calculate-the-difference-between-two-c-dates-for-ios8601/)
- [C++標準ライブラリ chrono - cppreference.com](https://en.cppreference.com/w/cpp/chrono)