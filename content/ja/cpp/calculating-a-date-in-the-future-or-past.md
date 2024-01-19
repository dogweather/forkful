---
title:                "未来または過去の日付を計算する"
html_title:           "C++: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 計算できる未来または過去の日付：なんでとなぜ？

未来または過去の日付を計算することは、指定された日付から特定の日数を追加または削除するという処理を意味します。プログラマは、時間地点の相対的な計算を行う、間隔を測定したり、期限を設定する等の多くの理由でこのような計算を行います。

# やり方：

以下に、C++で未来または過去の日付を計算するための基本的なコードを示します。

```C++
#include <ctime>
#include <iostream>

int main()
{
    std::time_t now = std::time(0);
    std::tm *ltm = std::localtime(&now);

    // 表示現在の日付 and 時間
    std::cout << "年: "<< 1900 + ltm->tm_year << "\n";
    std::cout << "月: "<< 1 + ltm->tm_mon << "\n";
    std::cout << "日: "<<  ltm->tm_mday << "\n";

    // サンプル: 15 日後の日付
    ltm->tm_mday += 15; 
    mktime(ltm);

    std::cout << "15 日後:\n";
    std::cout << "年: "<< 1900 + ltm->tm_year << "\n";
    std::cout << "月: "<< 1 + ltm->tm_mon << "\n";
    std::cout << "日: "<<  ltm->tm_mday << "\n";

    return 0;
}
```
このコードの出力は以下のようになります。

```
年: 2022
月: 5
日: 17
15 日後:
年: 2022
月: 6
日: 1
```
# 深堀り：

過去の日付を計算することは古代から存在します。年号と月日は 土地や文化によりますが、常に使っていた。 

C++で未来または過去の日付を計算するにはいくつかの方法があります。最も一般的なのは、`std::tm`と`std::mktime`を使用する方法ですが、他のライブラリーや方法が存在します。例えば、`boost::date_time`ライブラリーまたは`std::chrono`ライブラリーがあります。

`std::mktime`の役割は、`std::tm`構造体を`std::time_t`タイプに変換することです。もし`std::tm`構造体の値が範囲外だと（例えば、日付が31より大きいなど）、`std::mktime`は適切に日付を調節します。

# 参考情報：

- C++リファレンス：[std::mktime](http://www.cplusplus.com/reference/ctime/mktime/)
- C++リファレンス：[std::tm](http://www.cplusplus.com/reference/ctime/tm/)
- Boostライブラリーリファレンス：[Boost Date Time Library](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)