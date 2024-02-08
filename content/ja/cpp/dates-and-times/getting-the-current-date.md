---
title:                "現在の日付の取得"
date:                  2024-02-03T19:09:17.632072-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
C++で現在の日付を取得することは、システムの時計に基づいて日付を処理または表示する必要があるプログラムにとって基本的なタスクです。それは、ログ記録、タイムスタンピング、タスクスケジューリング、および日付と時間に依存するあらゆる機能にとって不可欠です。

## 方法：
C++では、C++標準ライブラリやBoostのようなサードパーティのライブラリを含む、現在の日付を取得するいくつかの方法が提供されています。次の例は、このタスクを達成する方法を示しています。

### `<chrono>`を使用する（C++20以降）
C++20は、 `<chrono>`ライブラリにより多くの機能を導入し、現在の日付を取得することを簡単にしました：
```cpp
#include <iostream>
#include <chrono>
#include <format> // std::format用 (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // 現在の時刻を取得
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // time_tに変換

    // 時間を読みやすい形式にフォーマットする
    std::cout << "現在の日付: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**サンプル出力:**
```plaintext
現在の日付: 2023-03-15
```

### `<ctime>`を使用する
C++の古いバージョンで作業しているプログラマーや、伝統的なCライブラリを好む人のために：
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // 現在の時刻を取得
    std::tm* now = std::localtime(&t);
    std::cout << "現在の日付: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**サンプル出力:**
```plaintext
現在の日付: 2023-03-15
```

### Boost Date_Timeを使用する
Boostライブラリを使用するプロジェクトのために、Boost Date_Timeライブラリは、現在の日付を取得する代替方法を提供します：
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Boostのグレゴリオ暦を使用して現在の日を取得する
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "現在の日付: " << today << std::endl;

    return 0;
}
```
**サンプル出力:**
```plaintext
現在の日付: 2023-Mar-15
```
これらの例は、幅広いアプリケーションでC++で日付の操作を行うための基礎を提供します。
