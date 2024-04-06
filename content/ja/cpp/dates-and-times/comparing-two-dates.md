---
date: 2024-01-20 17:32:41.018397-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.457119-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6B74\u53F2\u7684\u80CC\u666F\u3067\u306F\u3001C++\u306B\
  \u306F\u9577\u3044\u9593`<ctime>`\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u65E5\u6642\
  \u3092\u6271\u3046\u4E3B\u6D41\u3067\u3057\u305F\u304C\u3001C++11\u304B\u3089`<chrono>`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u304C\u5C0E\u5165\u3055\u308C\u3001\u3088\u308A\u7CBE\u78BA\
  \u3067\u6271\u3044\u3084\u3059\u3044API\u304C\u63D0\u4F9B\u3055\u308C\u3066\u3044\
  \u307E\u3059\u3002\u307E\u305F\u3001Boost.Date_Time\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306E\u3088\u3046\u306A\u4ED6\u306E\u9078\u629E\u80A2\u3082\u3042\u308A\u307E\u3059\
  \u304C\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u6A5F\u80FD\u62E1\u5F35\
  \u304C\u9032\u3080\u306B\u3064\u308C\u3066\u3001\u3088\u308A\u7A00\u306A\u30CB\u30FC\
  \u30BA\u306B\u5BFE\u51E6\u3059\u308B\u305F\u3081\u306E\u3082\u306E\u3068\u306A\u308A\
  \u3064\u3064\u3042\u308A\u307E\u3059\u3002\u5B9F\u88C5\u306E\u8A73\u7D30\u3067\u306F\
  \u3001`<chrono>`\u306F\u6642\u9593\u306E\u30DD\u30A4\u30F3\u30C8\u3092`time_point`\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u3068\u3057\u3066\u8868\u73FE\u3057\u3001\u6642\u9593\
  \u306E\u9593\u9694\u3084\u671F\u9593\u3092`duration`\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u3068\u3057\u3066\u6271\u3044\u307E\u3059\u3002\u3053\u308C\u306B\u3088\u308A\
  \u3001\u65E5\u4ED8\u306E\u6BD4\u8F03\u306F\u7570\u306A\u308B\u6642\u9593\u30DD\u30A4\
  \u30F3\u30C8\u9593\u306E`duration`\u3092\u8A08\u7B97\u3059\u308B\u3053\u3068\u3067\
  \u884C\u3048\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## How to: (やり方)
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // 現在のシステムの日時を取得
    auto now = std::chrono::system_clock::now();

    // tm構造体に変換
    std::time_t now_c = std::chrono::system_clock::to_time_t(now - std::chrono::hours(24));
    std::tm* now_tm = std::localtime(&now_c);
    
    // 昨日の日付を作成
    std::tm yesterday_tm = *now_tm;

    // 今日の日付を作成
    std::tm today_tm = *std::localtime(&now_c);
    today_tm.tm_hour = 0;
    today_tm.tm_min = 0;
    today_tm.tm_sec = 0;
    
    // tm構造体をtime_tに変換
    std::time_t yesterday = std::mktime(&yesterday_tm);
    std::time_t today = std::mktime(&today_tm);

    // 日付を比較
    if (std::difftime(today, yesterday) > 0) {
        std::cout << "Today is after Yesterday.\n";
    } else {
        std::cout << "Yesterday is not after Today.\n";
    }

    return 0;
}
```
サンプル出力:
```
Today is after Yesterday.
```

## Deep Dive (深掘り)
歴史的背景では、C++には長い間`<ctime>`ライブラリが日時を扱う主流でしたが、C++11から`<chrono>`ライブラリが導入され、より精確で扱いやすいAPIが提供されています。また、Boost.Date_Timeライブラリのような他の選択肢もありますが、標準ライブラリの機能拡張が進むにつれて、より稀なニーズに対処するためのものとなりつつあります。実装の詳細では、`<chrono>`は時間のポイントを`time_point`オブジェクトとして表現し、時間の間隔や期間を`duration`オブジェクトとして扱います。これにより、日付の比較は異なる時間ポイント間の`duration`を計算することで行えます。

## See Also (関連情報)
- [std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [std::ctime](https://en.cppreference.com/w/cpp/header/ctime)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
