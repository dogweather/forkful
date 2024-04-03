---
date: 2024-01-20 17:32:41.018397-07:00
description: "\u6BD4\u8F03\u3059\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u3068\u306F\u3001\
  \u5358\u7D14\u306B\u305D\u308C\u3089\u304C\u540C\u3058\u304B\u3001\u3069\u3061\u3089\
  \u304C\u5148\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\
  \u308C\u306F\u3001\u671F\u9650\u306E\u7BA1\u7406\u3001\u4E88\u7D04\u30B7\u30B9\u30C6\
  \u30E0\u3001\u5C65\u6B74\u30C7\u30FC\u30BF\u5206\u6790\u306A\u3069\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u3088\u304F\u51FA\u304F\u308F\u3059\u4F5C\u696D\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.571726-06:00'
model: gpt-4-1106-preview
summary: "\u6BD4\u8F03\u3059\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u3068\u306F\u3001\
  \u5358\u7D14\u306B\u305D\u308C\u3089\u304C\u540C\u3058\u304B\u3001\u3069\u3061\u3089\
  \u304C\u5148\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\
  \u308C\u306F\u3001\u671F\u9650\u306E\u7BA1\u7406\u3001\u4E88\u7D04\u30B7\u30B9\u30C6\
  \u30E0\u3001\u5C65\u6B74\u30C7\u30FC\u30BF\u5206\u6790\u306A\u3069\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u3088\u304F\u51FA\u304F\u308F\u3059\u4F5C\u696D\
  \u3002."
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## What & Why? (何となぜ？)
比較する二つの日付とは、単純にそれらが同じか、どちらが先かを判断することです。これは、期限の管理、予約システム、履歴データ分析など、プログラミングでよく出くわす作業。

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
