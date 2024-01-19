---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

プログラマが現在の日付を取得するのは、特定のイベントが起きた日をトラッキングするため、または日付に依存する機能をプログラムに実装するためです。これは、ログを出力するときやトランザクションのタイムスタンプを追跡する際に特に便利です。

## 使い方:

C++の`<chrono>`や`<ctime>`ライブラリを使って現在の日付を取得する基本的な例を見てみましょう。

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    std::time_t now_c = std::chrono::system_clock::to_time_t(now - std::chrono::hours(24));
    std::cout << "Yesterday was " << std::ctime(&now_c);
    return 0;
}
```
実行結果は以下の通りです:
```
Yesterday was Sun Jul  4 16:48:52 2022
```
このコードは、現在の日付と時間を取得し、それを24時間前にバックデートすることで「昨日」を取得します。

## 深堀り

以前は`ctime`ライブラリの`time()`と`localtime()`関数を用いて現在の日付と時間を取得していましたが、C++11から`chrono`ライブラリが導入され、より簡単かつ安全に時間計算が可能になりました。他の方法として、オペレーティングシステム固有のAPIを使用する方法もありますが、これはプラットフォーム間での移植性に問題があります。

## 関連情報

関連する情報については以下のリンクをご参照ください:

1. `<chrono>`ライブラリの詳細: https://en.cppreference.com/w/cpp/chrono
2. `<ctime>`ライブラリの詳細: https://en.cppreference.com/w/cpp/chrono/c
3. C++の日付と時間についての更なる情報: https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm