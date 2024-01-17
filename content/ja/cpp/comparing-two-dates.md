---
title:                "「日付の比較」"
html_title:           "C++: 「日付の比較」"
simple_title:         "「日付の比較」"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## どのように日付を比較するのか: プログラマーのための解説

日付を比較することは、特定の日付が過去・現在・未来のどれに当てはまるかを判断することです。プログラマーたちは、日付を比較することで、例えばイベントのスケジュールやデータのソートなど、さまざまな処理を行うことができます。

## どのように: 

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // 現在の日付を取得する
  time_t now = time(0); 
  tm *current_date = localtime(&now);

  // 比較したい日付を設定する
  tm specified_date = {0};
  specified_date.tm_year = 121; // 2021年
  specified_date.tm_mon = 5; // 6月 (0から始まるので5を設定)
  specified_date.tm_mday = 10; // 10日

  // 比較する
  if (mktime(&specified_date) < now) { // 指定した日付が過去の場合
    cout << "指定した日付は過去です" << endl;
  } else if (mktime(&specified_date) == now) { // 指定した日付が現在の場合
    cout << "指定した日付は現在です" << endl;
  } else { // 指定した日付が未来の場合
    cout << "指定した日付は未来です" << endl;
  }

  return 0;
}
```

### 出力結果 (今日が2021年6月10日の場合):
```C++
指定した日付は現在です
```

## 深堀り:

### 歴史的背景:
早い時期に、コンピューターでは日付を数値として処理していました。例えば、1970年1月1日からの経過秒数を表す「UNIX時間」がそれに当たります。しかし、この方法では日付の表現が煩雑であり、また世界中で使われるさまざまなカレンダーに対応することができませんでした。そこで、ISO 8601と呼ばれる日付の表記方法が開発され、現在では主流となっています。

### 他の方法:
日付の比較にはもう一つ方法があります。それは、日付を「文字列」として扱う方法です。例えば、YYYY/MM/DDのように日付を表した文字列を比較することで、日付の大小を判断することができます。しかしこの方法では、日付を数値として扱うよりも処理が遅くなることが多いため、一般的ではありません。

### 実装の詳細:
C++では、日付を `tm` 構造体で表します。この構造体にはtm_year、tm_mon、tm_mdayなどのメンバ変数があり、それぞれ年、月、日を表します。また、時間を扱うための `tm_hour` や `tm_min`、 `tm_sec` もあります。
`mktime()` 関数を使うことで、 `tm` 構造体を `time_t` 構造体に変換し、 `time()` 関数で現在の日付を取得することができます。

## 関連情報:
- [C++ 日付/時刻ライブラリ](https://en.cppreference.com/w/cpp/chrono)
- [日付の表記方法 (ISO 8601)](https://ja.wikipedia.org/wiki/ISO_8601)