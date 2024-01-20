---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付の比較とは、2つの日付間の差を確認することを指します。この過程は、予定を計画したり、期日を追跡したり、間隔を測定したりするためにプログラマーによって頻繁に使用されます。

## 使い方:

```C
#include<stdio.h>
#include<time.h>

int main() {
    struct tm a = {0,0,0,30,11,120,0,0,0}; // December 30, 2020
    time_t x = mktime(&a);

    struct tm b = {0,0,0,1,11,120,0,0,0}; // December 1, 2020
    time_t y = mktime(&b);

    double difference = difftime(x,y);
    printf("%.f", difference); // Displays the difference in seconds
    
    return 0;
}
``` 
これは、2020年12月30日と2020年12月1日の差を秒単位で表示するプログラムです。出力は2498400となります。

## 深掘り

1. 歴史的な文脈：
The C言語は、UNIXオペレーティングシステムを実装するために1970年代初頭に開発された。日付と時刻は `<time.h>` ライブラリを使用して操作されます。

2. 代替案：
日付比較のための他の方法は、心地よい開発者独自の関数を作成することです。またdatetimeライブラリを使用したPython、JavaのDateクラスなど、他のプログラミング言語が提供する日付比較メソッドも考慮できます。

3. 実装の詳細：
`mktime()`関数は、 struct tmを引数とし、これをtime_t形式に変換して返します。`difftime()`関数は、二つの時間を比較し、二つの時間間隔の差異を返します。

## 関連情報

* [Cライブラリ-time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
* [Pythonのdatetimeライブラリ](https://docs.python.org/ja/3/library/datetime.html)
* [Java Date Class](https://www.javatpoint.com/java-date)