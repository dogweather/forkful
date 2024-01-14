---
title:                "C++: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

今日の日付を取得することに興味があるかもしれません。プログラムを実行する日付に基づいてアクションを実行する場合や、日付をログに記録する必要がある場合など、様々な理由があるかもしれません。

## 方法

C++には、現在の日付を取得するための便利な関数があります。それが「std::chrono::system_clock::now()」です。以下のコードを書いてみましょう。

```C++
#include <iostream>
#include <chrono> // 日付に関するライブラリをインポート

int main() {
  auto current_time = std::chrono::system_clock::now(); // 現在の時刻を取得
  time_t time = std::chrono::system_clock::to_time_t(current_time); // tm構造体に変換
  std::cout << "今日の日付は" << std::ctime(&time) << "です" << std::endl; // 出力
  return 0;
}
```

このコードを実行すると、以下のような出力が得られます。

```C++
今日の日付はWed Dec 16 15:52:10 2020です
```

このコードでは、現在の日付を「std::chrono::system_clock::now()」で取得し、それを時刻情報を持つtm構造体に変換しています。「std::ctime()」を使用することで、tm構造体を文字列に変換し、出力しています。

## ディープダイブ

現在の日付を取得するための方法にはさまざまな種類があります。「std::chrono::system_clock」以外にも、「std::chrono::steady_clock」や「std::chrono::high_resolution_clock」もあります。「std::chrono::system_clock」はシステムのリアルタイム時計を使用するのに対し、「std::chrono::steady_clock」はプログラム起動後の経過時間を使用します。「std::chrono::high_resolution_clock」は最も精度の高いクロックを使用しますが、利用可能性は環境によって異なります。

また、日付と時刻の装飾方法も多様です。「std::ctime()」の代わりに「std::put_time()」を使用することで、より細かいフォーマット指定が可能になります。詳しくはC++の日付と時刻の文法について調べることをおすすめします。

## See Also

- [C++の日付と時刻の文法](https://cpprefjp.github.io/reference/chrono/)
- [std::chronoライブラリについて](https://cpprefjp.github.io/reference/chrono.html)
- [tm構造体について](https://cpprefjp.github.io/reference/ctime/tm.html)
- [「std::chrono::system_clock」について](https://cpprefjp.github.io/reference/chrono/system_clock.html)