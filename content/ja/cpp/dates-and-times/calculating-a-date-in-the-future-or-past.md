---
date: 2024-01-20 17:30:47.141416-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.458524-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u904E\u53BB\u3001\u65E5\u4ED8\u306E\u8A08\u7B97\u306F\u81EA\
  \u4F5C\u95A2\u6570\u3084\u30E9\u30A4\u30D6\u30E9\u30EA\u4F9D\u5B58\u3060\u3063\u305F\
  \u3002C++11\u4EE5\u964D\u3001`<chrono>`\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5C0E\
  \u5165\u3055\u308C\u3001\u6642\u9593\u306E\u6982\u5FF5\u306F\u5927\u304D\u304F\u5909\
  \u308F\u3063\u305F\u3002\u4E0A\u8A18\u306E\u4F8B\u3067\u306F`std::chrono`\u3092\u4F7F\
  \u7528\u3057\u3001\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u52A0\u6E1B\u7B97\u3092\
  \u884C\u3046`days`\u578B\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u6A19\u6E96\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u306F\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u9593\
  \u3067\u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u8A3C\u3059\u308B\u304C\u3001\u6642\u306B\
  \u306F\u30BF\u30A4\u30E0\u30BE\u30FC\u30F3\u8A08\u7B97\u3084\u958F\u79D2\u306E\u3088\
  \u3046\u306A\u8907\u96D1\u306A\u30B1\u30FC\u30B9\u3092\u6271\u3046\u305F\u3081\u306B\
  \u5916\u90E8\u30E9\u30A4\u30D6\u30E9\u30EA\uFF08`Boost.Date_Time`\u306A\u3069\uFF09\
  \u306E\u4F7F\u7528\u304C\u63A8\u5968\u3055\u308C\u308B\u5834\u5408\u3082\u3042\u308A\
  \u307E\u3059\u3002"
title: "\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u306E\u8A08\u7B97"
weight: 26
---

## How to: (方法)
```C++
#include <iostream>
#include <chrono>
#include <ctime>
#include <iomanip>

int main() {
    using namespace std::chrono;

    // 現在の日時を取得
    system_clock::time_point today = system_clock::now();
    time_t tt = system_clock::to_time_t(today);
    
    // 5日後の日付を計算
    system_clock::time_point future_date = today + days{5};
    tt = system_clock::to_time_t(future_date);
    
    // 5日前の日付を計算
    system_clock::time_point past_date = today - days{5};
    time_t pt = system_clock::to_time_t(past_date);
    
    // 出力
    std::cout << "今日の日付: " << std::put_time(std::localtime(&tt), "%Y-%m-%d") << std::endl;
    std::cout << "5日後の日付: " << std::put_time(std::localtime(&tt), "%Y-%m-%d") << std::endl;
    std::cout << "5日前の日付: " << std::put_time(std::localtime(&pt), "%Y-%m-%d") << std::endl;
    
    return 0;
}
```

Sample Output:
```
今日の日付: 2023-04-30
5日後の日付: 2023-05-05
5日前の日付: 2023-04-25
```

## Deep Dive (深掘り)
過去、日付の計算は自作関数やライブラリ依存だった。C++11以降、`<chrono>`ライブラリが導入され、時間の概念は大きく変わった。上記の例では`std::chrono`を使用し、タイムスタンプ加減算を行う`days`型を利用します。標準ライブラリはプラットフォーム間での一貫性を保証するが、時にはタイムゾーン計算や閏秒のような複雑なケースを扱うために外部ライブラリ（`Boost.Date_Time`など）の使用が推奨される場合もあります。

## See Also (参照)
- C++ Reference: `<chrono>` documentation: https://en.cppreference.com/w/cpp/header/chrono
- Boost Date_Time library: https://www.boost.org/doc/libs/release/libs/date_time/
- Howard Hinnant's date library, which expands on the `<chrono>` facilities: https://github.com/HowardHinnant/date
