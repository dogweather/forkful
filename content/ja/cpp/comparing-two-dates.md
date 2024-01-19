---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付の比較とは、2つの日付を比べ、その結果が最初が二番目より早い、遅い、または同じなのかを判断するプロセスです。これは、何かが何時に起こったか、期限が来る前に任務が完了したかどうかを決定するために、プログラマーによって行われます。

## 方法：
C++を使った日付の比較例を以下に示します。

```C++
#include <ctime>
#include <iostream>

int main() {
    // 現在日付を取得します。
    std::time_t now = std::time(nullptr);
    struct std::tm timeinfo_now = *std::localtime(&now);

    // 来年の同じ日付を設定します。
    struct std::tm timeinfo_next_year = timeinfo_now;
    timeinfo_next_year.tm_year++;

    // mktime関数でtmをtime_tに変換します。
    std::time_t t_now = std::mktime(&timeinfo_now);
    std::time_t t_next_year = std::mktime(&timeinfo_next_year);

    if(t_now < t_next_year) {
        std::cout << "Now is earlier." << std::endl;
    } else if(t_now == t_next_year) {
        std::cout << "Dates are identical." << std::endl;
    } else {
        std::cout << "Now is later." << std::endl;
    }

    return 0;
}
```
この-outputは「Now is earlier.」です。

## ディープダイブ：
もっと深く見てみましょう。C++の初期のバージョンでは、日付の比較は直感的ではありませんでしたが、`<ctime>`ライブラリの導入とともに、これが大幅に改善されました。 

代替案としては、`<chrono>`ライブラリを考えます。これはC++11以降で使用できます。 `std::chrono::system_clock::time_point`を使用して日付を比較することができます。

実装の詳細については、`mktime`関数は引数として与えられた`tm`構造体を正規化し、それを`time_t`型の時間に変換します。これは、日付や時間の単位が適切な範囲内に収まるように調整します（例えば、`tm_hour`が24以上の場合など）。

## 参考リンク：
- C++ `<ctime>`プライマリでの日付と時間の操作についてはこちら： [http://www.cplusplus.com/reference/ctime/](http://www.cplusplus.com/reference/ctime/)
- C++ `<chrono>` ライブラリの詳細はこちら: [http://www.cplusplus.com/reference/chrono/](http://www.cplusplus.com/reference/chrono/)
- 日付比較のさまざまな方法については、[https://stackoverflow.com/questions/55550/comparing-dates-in-c](https://stackoverflow.com/questions/55550/comparing-dates-in-c) を参照してください。