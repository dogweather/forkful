---
title:                "未来や過去の日付の計算"
html_title:           "C++: 未来や過去の日付の計算"
simple_title:         "未来や過去の日付の計算"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

日付を未来や過去に計算するとは、特定の日付から一定の期間を加えたり減らしたりすることを意味します。プログラマーがこれを行う主な理由は、プログラム内で特定の日付に関する処理を行うためです。

## 方法：

```C++
// 未来の日付を計算する
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // 現在の日付を取得
    time_t now = time(0); 
    tm* ltm = localtime(&now);

    // 未来の日付を計算する
    ltm->tm_mday += 7;
    mktime(ltm);

    // 出力
    cout << "1週間後は" << ltm->tm_year + 1900 << "年" << ltm->tm_mon + 1 << "月" << ltm->tm_mday << "日です。" << endl;
    return 0;
}

// 出力： 1週間後は 2021年 7月 1日です。
```

```C++
// 過去の日付を計算する
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // 現在の日付を取得
    time_t now = time(0); 
    tm* ltm = localtime(&now);

    // 過去の日付を計算する
    ltm->tm_mday -= 3;
    mktime(ltm);

    // 出力
    cout << "3日前は" << ltm->tm_year + 1900 << "年" << ltm->tm_mon + 1 << "月" << ltm->tm_mday << "日です。" << endl;
    return 0;
}

// 出力： 3日前は 2021年 6月 25日です。
```

## 詳細を掘り下げる：

(1) 歴史的背景：日付の計算は非常に古く、太陽暦の発展とともに進化してきました。現代では、コンピューターの発展によりより高度な計算が可能になりました。

(2) 代替案：日付の計算には様々な方法がありますが、最もよく使われる方法は標準ライブラリの`ctime`を使用する方法です。

(3) 実装の詳細：上記の例では、現在の日付を取得し、`tm`構造体を介して日付の要素を操作しました。そして、`mktime`を使用して更新された日付を元に新しい日付を計算しました。最後に、`cout`を使用して日付の情報を出力しました。

## 関連ソース：

- [C++ Reference: Date and time utilities](https://en.cppreference.com/w/cpp/chrono/c)
- [C++ Tutorial: Date and Time in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [Microsoft Docs: Working with Dates and Times in C++](https://docs.microsoft.com/en-us/cpp/standard-library/working-with-dates-and-times-in-cpp?view=msvc-160)