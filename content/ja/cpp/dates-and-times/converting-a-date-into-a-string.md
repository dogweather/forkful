---
title:                "日付を文字列に変換する"
aliases:
- /ja/cpp/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:18.686476-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

日付を文字列に変換することは、日付データを読みやすく表示するプロセスです。プログラマーは通常、ユーザーインターフェースで日付を表示するためや、ファイル名、ログ、または通信プロトコルで日付を使うためにこれを行います。

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
