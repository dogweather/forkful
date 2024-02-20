---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:17.632072-07:00
description: "C++\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u6642\u8A08\u306B\u57FA\u3065\
  \u3044\u3066\u65E5\u4ED8\u3092\u51E6\u7406\u307E\u305F\u306F\u8868\u793A\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308B\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3068\u3063\u3066\
  \u57FA\u672C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u305D\u308C\u306F\u3001\
  \u30ED\u30B0\u8A18\u9332\u3001\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D4\u30F3\u30B0\
  \u3001\u30BF\u30B9\u30AF\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u304A\
  \u3088\u3073\u65E5\u4ED8\u3068\u6642\u9593\u306B\u4F9D\u5B58\u3059\u308B\u3042\u3089\
  \u3086\u308B\u6A5F\u80FD\u306B\u3068\u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
lastmod: 2024-02-19 22:05:01.681366
model: gpt-4-0125-preview
summary: "C++\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u6642\u8A08\u306B\u57FA\u3065\u3044\
  \u3066\u65E5\u4ED8\u3092\u51E6\u7406\u307E\u305F\u306F\u8868\u793A\u3059\u308B\u5FC5\
  \u8981\u304C\u3042\u308B\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3068\u3063\u3066\u57FA\
  \u672C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u305D\u308C\u306F\u3001\u30ED\
  \u30B0\u8A18\u9332\u3001\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D4\u30F3\u30B0\u3001\
  \u30BF\u30B9\u30AF\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u304A\u3088\
  \u3073\u65E5\u4ED8\u3068\u6642\u9593\u306B\u4F9D\u5B58\u3059\u308B\u3042\u3089\u3086\
  \u308B\u6A5F\u80FD\u306B\u3068\u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
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
