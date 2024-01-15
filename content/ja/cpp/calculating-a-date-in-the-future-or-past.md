---
title:                "未来または過去の日付を計算する"
html_title:           "C++: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
日付を未来や過去に計算する理由は多々ありますが、例えば誕生日や結婚記念日などのイベントを確認するために使うことができます。また、スケジュールを管理する際にも役立ちます。

## 方法
以下のように、現在の日付から指定した日数分を加算したり、引いたりして、未来や過去の日付を計算することができます。

```C++
#include <iostream>
#include <chrono>  // 時間操作を行うライブラリ

using namespace std;
using namespace std::chrono;  // 時間操作を行うライブラリの名前空間を指定する

int main() {
    // 現在の日付を取得する
    system_clock::time_point today = system_clock::now();

    // 未来の日付を計算する (10日後)
    system_clock::time_point future = today + days(10);

    // 過去の日付を計算する (5日前)
    system_clock::time_point past = today - days(5);

    // 日付を表示する
    // "d"は日にちを表すフォーマット指定子
    cout << "未来の日付: " << format("%Y/%m/%d", future) << endl;
    cout << "過去の日付: " << format("%Y/%m/%d", past) << endl;

    return 0;
}
```

実行結果:

```
未来の日付: 2021/08/11
過去の日付: 2021/07/26
```

## ディープダイブ
さらに、特定の年や月、日を指定して日付を計算することもできます。例えば、結婚記念日から10年後の日付を計算する場合は、以下のようにコードを書くことができます。

```C++
#include <iostream>
#include <chrono>

using namespace std;
using namespace std::chrono;

int main() {
    // 結婚記念日を表す日付を設定
    system_clock::time_point anniversary = system_clock::from_time_t(1614637200);  // 2021/03/02 (UNIX時間で表す)

    // 結婚記念日から10年後の日付を計算する
    system_clock::time_point future = anniversary + years(10);

    // 日付を表示する
    cout << "結婚記念日から10年後の日付: " << format("%Y/%m/%d", future) << endl;

    return 0;
}
```

実行結果:

```
結婚記念日から10年後の日付: 2031/03/02
```

## 参考リンク
- [C++で日付を操作する方法](https://programmingblog.jp/articles/5209698)
- [C++日付ライブラリの使い方](https://qiita.com/yyama1016/items/a4d90c9df022608ea710)