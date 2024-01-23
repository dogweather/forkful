---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:13:26.902050-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (なに？そしてなぜ？)
現在の日付を取得するっていうのは、今日が何年何月何日かをプログラムで知ることさ。なぜそうするかって？ログ、レポート、時間依存の機能を実装するためだね。

## How to: (やり方)
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // chronoを使ってシステム時計から現在の時間を取得
    auto now = std::chrono::system_clock::now();
    // time_t型に変換
    std::time_t now_time = std::chrono::system_clock::to_time_t(now);
    
    // tm型に変換して読みやすくする
    std::tm* now_tm = std::localtime(&now_time);
    std::cout << "年: " << 1900 + now_tm->tm_year << "\n";
    std::cout << "月: " << 1 + now_tm->tm_mon << "\n";
    std::cout << "日: " << now_tm->tm_mday << std::endl;

    return 0;
}
```
出力例：
```
年: 2023
月: 2
日: 15
```

## Deep Dive (深掘り)
`<chrono>`ライブラリはC++11で導入されて、時間に関する強力な操作を扱えるようになった。`std::chrono::system_clock`はシステムの現在時刻を表す。`std::time_t`は古くからあるC言語の標準で、UNIXエポック（1970年1月1日からの秒数）を表す。C++では`std::tm`と組み合わせて年月日などを取り出すことが多い。

ちなみに、他にも日付や時間を取得する方法はある。`<ctime>`の`std::localtime`などが古いやり方だね。しかし`<chrono>`はより安全で、精度が高く、使いやすい。

具体的には、`std::chrono`系はタイムゾーンの問題や細かい時間の計算を扱う際に有利だ。時代と共にプログラミングも進化している、今日のC++では`<chrono>`を使うのが一般的だ。

## See Also (関連情報)
- [cppreference std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [cppreference std::time_t](https://en.cppreference.com/w/cpp/chrono/c/time_t)
- [cppreference std::tm](https://en.cppreference.com/w/cpp/chrono/c/tm)
