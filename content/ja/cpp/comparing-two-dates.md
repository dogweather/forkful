---
title:                "2つの日付の比較"
html_title:           "C++: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
日付を比較することは、日付データを操作するために必要な重要なスキルです。例えば、ある日付が別の日付よりも前なのか後なのかを判断する必要がある場合や、日付をソートする場合などに役立ちます。

## 方法
以下のようなコードを使って、2つの日付を比較する方法を説明します。
```C++
#include <iostream>
#include <ctime>

int main() {
    // 日付を表すtm構造体の作成
    std::tm day1 = { 0, 0, 12, 3, 5, 121}; // 12:00:00 March 3, 2021
    std::tm day2 = { 0, 0, 10, 3, 5, 121}; // 12:00:00 March 10, 2021
    
    // std::mktime関数を使ってtm構造体をtime_t型に変換
    std::time_t time1 = std::mktime(&day1);
    std::time_t time2 = std::mktime(&day2);

    if (time1 < time2)
        std::cout << "day1 is before day2";
    else if (time1 > time2)
        std::cout << "day2 is before day1";
    else
        std::cout << "day1 and day2 are the same";
    
    return 0;
}
```
上記のコードでは、2つの日付を別々のtm構造体に作成し、std::mktime関数を使ってtime_t型に変換してから比較しています。

以下は、コードの実行結果です。
```
day2 is before day1
```

## ディープダイブ
日付を比較する際に留意するべき点があります。大きく分けて以下の2つが挙げられます。

- 日付の表現方法: C++では日付を様々な形式で表現することができますが、それぞれの表現方法によって比較の仕方が異なります。上のコードでは、標準的なtm構造体を使って日付を表現しましたが、別の方法を使う場合は比較の方法も変える必要があります。
- タイムゾーンの考慮: タイムゾーンによって日付や時刻が異なるため、比較をする際にはタイムゾーンを考慮する必要があります。例えば、同じ日付でも日本とアメリカではタイムゾーンが異なるため、比較の結果が異なる可能性があります。

## 参考
- [C++で日付を扱うときに最適な方法](https://www.techscore.com/blog/2018/06/20/cpp-date/)
- [time_t型とtm構造体の使い方](https://www.geeksforgeeks.org/time-t-tm-structure-time-h-in-cpp/)
- [日付の比較を行うstd::mktime関数の使い方](https://cubix2433.hatenablog.com/entry/2019/02/02/114743)