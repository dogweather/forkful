---
title:                "C++: 「現在日付の取得」"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをはじめたばかりの方や、C++言語を使い始めたばかりの方にとって、現在の日付を取得することは初めての経験かもしれません。しかし、日付を取得することはプログラミングの基礎であり、日時を扱うアプリケーションを作る上でとても重要なスキルです。今回は、C++言語で日付を取得する方法について説明していきます。

## 方法

まずは、現在の日付を取得するためにはどのようなコードを書けば良いのかを見ていきましょう。C++言語では、標準ライブラリの`<ctime>`ヘッダーファイルを使用して日付を取得することができます。以下のコードを参考にしてください。

```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t current_time = std::time(0); // 現在の時間を取得
    std::tm* now = std::localtime(&current_time); // 現在のローカル時間を取得

    // 日付をフォーマットして出力
    std::cout << "今日は" << now->tm_year + 1900 << "年"
              << now->tm_mon + 1 << "月"
              << now->tm_mday << "日です。" << std::endl;

    return 0;
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
今日は2018年10月5日です。
```

このコードでは、`time()`関数を使用して現在の時間を秒単位で取得し、 `localtime()`関数を使用してその時間をローカル時間に変換しています。そして、`struct tm`を使用して日付をフォーマットして出力しています。

## 深堀り

日付を取得する際に、日付時刻を扱うためのさまざまな関数が存在します。例えば、`mktime()`関数を使用すると、`struct tm`の形式の日付を取得することができます。また、`strftime()`関数を使用して任意のフォーマットの日付を取得することもできます。

さらに、C++11からは`<chrono>`ヘッダーファイルが導入され、より柔軟な時間の扱いが可能になりました。`system_clock`クラスを使用すると、より高精度な時間を取得できます。

## 併せて読みたい

- [C++での日付と時刻の表現方法](https://www.ibm.com/developerworks/jp/linux/library/l-cpp-oop/time-and-date-cpp/index.html)
- [C++ 公式ドキュメント - <ctime>ヘッダ](http://www.cplusplus.com/reference/ctime/)
- [C++ 公式ドキュメント - <chrono>ヘッダ](http://www.cplusplus.com/reference/chrono/)

### 併せて読みたい