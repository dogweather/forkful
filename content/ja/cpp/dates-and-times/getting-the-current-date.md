---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:17.632072-07:00
description: "\u65B9\u6CD5\uFF1A C++\u3067\u306F\u3001C++\u6A19\u6E96\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3084Boost\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u542B\u3080\u3001\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\
  \u304C\u63D0\u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u6B21\u306E\u4F8B\u306F\
  \u3001\u3053\u306E\u30BF\u30B9\u30AF\u3092\u9054\u6210\u3059\u308B\u65B9\u6CD5\u3092\
  \u793A\u3057\u3066\u3044\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:42.568139-06:00'
model: gpt-4-0125-preview
summary: "C++\u3067\u306F\u3001C++\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3084\
  Boost\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u542B\u3080\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3092\
  \u53D6\u5F97\u3059\u308B\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u304C\u63D0\u4F9B\
  \u3055\u308C\u3066\u3044\u307E\u3059\u3002\u6B21\u306E\u4F8B\u306F\u3001\u3053\u306E\
  \u30BF\u30B9\u30AF\u3092\u9054\u6210\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u3066\
  \u3044\u307E\u3059."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
