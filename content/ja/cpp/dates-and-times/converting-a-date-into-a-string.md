---
date: 2024-01-20 17:36:18.686476-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.780800-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u65E5\u4ED8\u3068\u6642\u523B\u306E\u64CD\u4F5C\u306FC++\u4EE5\
  \u524D\u304B\u3089\u3042\u308B\u8907\u96D1\u306A\u30C8\u30D4\u30C3\u30AF\u3067\u3059\
  \u3002\u521D\u671F\u306EC\u3067\u306F`<ctime>`\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\
  \u4F7F\u308F\u308C\u3066\u3044\u307E\u3057\u305F\u3002\u3057\u304B\u3057\u3001C++11\u4EE5\
  \u964D\u3001`<chrono>`\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u30E2\u30C0\u30F3\u306A\
  \u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u63D0\u4F9B\u3057\u3001\u3088\u308A\u5B89\u5168\
  \u3067\u4F7F\u3044\u3084\u3059\u304F\u306A\u3063\u3066\u3044\u307E\u3059\u3002\u4EE3\
  \u66FF\u624B\u6BB5\u3068\u3057\u3066\u306F\u3001`strftime`\u95A2\u6570\u3084\u30AB\
  \u30B9\u30BF\u30E0\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u95A2\u6570\u3092\u4F7F\u3046\
  \u3053\u3068\u3082\u53EF\u80FD\u3067\u3059\u304C\u3001`<chrono>`\u3068`std::put_time`\u306E\
  \u7D44\u307F\u5408\u308F\u305B\u306F\u5F37\u529B\u3067\u3059\u3002\u5B9F\u88C5\u306E\
  \u8A73\u7D30\u3067\u306F\u30BF\u30A4\u30E0\u30BE\u30FC\u30F3\u3084\u30ED\u30B1\u30FC\
  \u30EB\uFF08\u5730\u57DF\u8A2D\u5B9A\uFF09\u3078\u306E\u5BFE\u5FDC\u3082\u91CD\u8981\
  \u306B\u306A\u308A\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (方法)
```C++
#include <iostream>
#include <iomanip>
#include <sstream>
#include <chrono>

int main() {
    // 現在のシステム時刻を取得
    auto now = std::chrono::system_clock::now();
    // 時刻を時間に変換
    std::time_t time_now = std::chrono::system_clock::to_time_t(now);
    // tm構造体に変換
    std::tm* ptm = std::localtime(&time_now);

    // 文字列ストリームを使って日付の文字列をフォーマット
    std::stringstream ss;
    ss << std::put_time(ptm, "%Y-%m-%d %H:%M:%S"); // ISO 8601形式

    // 結果の文字列を取得
    std::string datetime = ss.str();

    std::cout << datetime << std::endl; // 例: "2023-03-28 12:45:59"
    return 0;
}
```

## Deep Dive (掘り下げ)
日付と時刻の操作はC++以前からある複雑なトピックです。初期のCでは`<ctime>`ライブラリが使われていました。しかし、C++11以降、`<chrono>`ライブラリがモダンなアプローチを提供し、より安全で使いやすくなっています。代替手段としては、`strftime`関数やカスタムフォーマット関数を使うことも可能ですが、`<chrono>`と`std::put_time`の組み合わせは強力です。実装の詳細ではタイムゾーンやロケール（地域設定）への対応も重要になります。

## See Also (関連リンク)
- C++ `chrono` library: https://en.cppreference.com/w/cpp/chrono
- C++ `<iomanip>` library: https://en.cppreference.com/w/cpp/io/manip
- C++ Time formatting with `std::put_time`: https://en.cppreference.com/w/cpp/io/manip/put_time
- Historical context of time in C++: https://www.stroustrup.com/C++11FAQ.html#std-chrono
