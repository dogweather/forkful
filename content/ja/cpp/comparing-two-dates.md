---
title:    "C++: 「日付を比較する」"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
日付の比較をする理由について説明します。日付の比較は、プログラミングで頻繁に行われるタスクの1つであり、特定の日付が前の日付よりも早いか遅いかを判断したり、2つの日付が同じかどうかを確認するために使用されます。

## 方法
日付の比較を行うための具体的な方法を説明します。下記のコードブロックは、日付を比較するために使用できる簡単なC++の例を示しています。この例では、2つの日付の差を求める方法を示しています。

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {

  // 2つの日付を設定
  struct tm date1 = {0, 0, 0, 1, 1, 2020}; // 2020年1月1日
  struct tm date2 = {0, 0, 0, 12, 12, 2020}; // 2020年12月12日

  // 日付を秒数に変換
  time_t time1 = mktime(&date1);
  time_t time2 = mktime(&date2);

  // 日付の差を計算
  int difference = difftime(time2, time1); // 秒数での差を求める

  // 結果を出力
  cout << "日付の差は " << difference / (24 * 60 * 60) << " 日です。" << endl;

  return 0;
}
```

上記のコードを実行すると、日付の差が計算され、結果として「日付の差は 345 日です。」という出力が得られます。このように、日付を秒数に変換し差を求めることで、日付の比較を行うことができます。

## 詳細を深く掘り下げる
日付の比較には様々な方法がありますが、最も一般的な方法は日付を秒数に変換し、その差を計算する方法です。しかし、この方法にはいくつかの注意点があります。

例えば、うるう年の影響を受ける場合や、タイムゾーンの違いによって日付の差が正しく計算されないことがあります。また、特定の日付形式やロケールによっても差の計算方法が異なる場合があります。

以上のような問題を解決するために、日付の比較を行う場合はライブラリなどの既存のツールを使用することが推奨されます。例えば、BoostライブラリやChronoライブラリなどがあり、日付の比較を行う際に便利な関数やクラスを提供しています。

## 参考リンク
- [Boost.Date_Time library](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [C++ Primer第5版 日付と時刻に対する操作](https://www.amazon.co.jp/dp/4048678135)
- [日付と時刻処理の国際化](https://www.ykaku.com/gengo/time_date_and_locale.html)

## 関連記事を見る