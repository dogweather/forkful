---
title:    "C++: 今日の日付を取得する。"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
プログラムを書く際、現在の日付を取得する必要があることがあります。例えば、レポートを自動的に日付付きで保存するためや、イベントの日付を表示するために使用することができます。

## 作り方
日付を取得するには、```std::chrono::system_clock```を使用します。```std::chrono::system_clock```は、現在の日付と時刻を取得するための標準ライブラリです。

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // 現在の日付と時刻を取得する
    auto now = std::chrono::system_clock::now();

    // タイムスタンプを取得する
    std::time_t timestamp = std::chrono::system_clock::to_time_t(now);

    // フォーマットを指定して文字列に変換する
    char buffer[80];
    std::strftime(buffer, 80, "%Y/%m/%d", std::localtime(&timestamp));

    // 結果を出力する
    std::cout << buffer << std::endl;

    return 0;
}
```

**出力:**
```
2021/01/01
```

## ディープダイブ
```std::chrono::system_clock::now()```は、```std::chrono::time_point```オブジェクトを返します。このオブジェクトには、現在の日付と時刻のキャスト方法や加算や減算などの様々な操作が可能です。

また、```std::chrono::system_clock```は、UTC時間を返すため、必要に応じてタイムゾーンの変換を行う必要があります。これには、```std::localtime()```や```std::gmtime()```を使用することができます。

## 参考リンク
- [時間を扱うためのC++標準ライブラリ](https://www.fluentcpp.com/2018/01/12/times-and-dates-cpp/)
- [std::chrono::system_clockのドキュメント](https://en.cppreference.com/w/cpp/chrono/system_clock)
- [std::strftimeのドキュメント](https://en.cppreference.com/w/cpp/chrono/c/strftime)