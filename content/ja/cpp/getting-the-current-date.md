---
title:                "現在の日付を取得する"
html_title:           "C++: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
日付がいつなのかを知りたい人はたくさんいます。プログラムで日付を取得することで、その日付を使用してさまざまなことができます。例えば、予定を管理したり、データを分析したり、日付に基づいて機能を有効にしたりすることができます。

## 方法
```C++
#include <iostream>
#include <ctime>

int main() {
    
    // 現在の日付を取得する
    time_t now = time(0);
    
    // 構造体に変換する
    tm* timeinfo = localtime(&now);
    
    // 年、月、日を取得する
    int year = 1900 + timeinfo->tm_year;
    int month = 1 + timeinfo->tm_mon;
    int day = timeinfo->tm_mday;
    
    // 結果を出力する
    std::cout << "今日の日付は " << year << "年" << month << "月" << day << "日です。\n";
    
    return 0;
}
```
出力結果：
```
今日の日付は 2021年8月16日です。
```

## 深堀り
日付を取得するには、C++標準ライブラリの「ctime」ヘッダーを使用します。その中にある「time」関数を使用することで、現在の日付を取得することができます。また、取得した日付を構造体に変換することで、年月日だけでなく、曜日や時刻などの情報も取得することができます。

## 関連リンク
- [C++ ctimeヘッダー](https://www.cplusplus.com/reference/ctime/)
- [C++ time関数の使い方](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [C++標準ライブラリの使い方](https://cpprefjp.github.io/)