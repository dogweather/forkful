---
date: 2024-01-20 17:30:47.141416-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.573075-06:00'
model: gpt-4-1106-preview
summary: .
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
