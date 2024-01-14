---
title:    "C++: 将来または過去の日付の計算"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を計算する必要はありますか？プログラミングで日付を計算する理由は様々です。例えば、特定の日付のイベントを予定したり、将来の予定を調整したりするために使用することができます。

## 方法

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // 現在の日付を取得
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    
    // 日数を指定して未来の日付を計算
    std::chrono::duration<int> one_day (24*60*60);
    std::chrono::system_clock::time_point future = today + one_day * 3;

    // 様々なフォーマットで日付を表示
    std::time_t tt = std::chrono::system_clock::to_time_t(future);
    std::cout << "3日後の日付は: " << ctime(&tt) << std::endl;

    return 0;
}
```

上記のコードでは、標準ライブラリである`chrono`を使用して現在の日付を取得し、日数を指定して未来の日付を計算しています。その後、`ctime`を用いて日付を様々なフォーマットで表示しています。このように、C++では簡単に日付を計算することができます。

## 詳細

C++では、日付を計算するために様々なライブラリが利用できます。例えば、上記のコードでは`chrono`を使用しましたが、`boost`や`date`ライブラリを使用することもできます。また、日付の計算だけでなく、日付の比較や変換なども可能です。

より高度な日付の計算をする場合は、外部ライブラリを使用するか、特定のアルゴリズムを学ぶ必要があります。また、タイムゾーンや特殊な日付フォーマットについても理解する必要があります。

ではなぜC++を使用して日付を計算する必要があるのでしょうか？それは、より速く、高機能で、柔軟なプログラムを作るためです。C++はパフォーマンスが高いことが特徴の一つであり、日付計算にもその長所を生かすことができます。また、C++はオブジェクト指向言語であるため、日付操作をオブジェクト指向的にプログラムすることができます。

## 関連リンク

- [C++で日付を扱う方法](https://www.geeksforgeeks.org/date-time-in-c/) 
- [boostライブラリの日付操作機能](https://www.boost.org/doc/libs/1_66_0/doc/html/date_time.html) 
- [Rarecharmさんによる日付計算のアルゴリズムの紹介](https://www.nayuki.io/page/next-lexicographical-permutation-algorithm)