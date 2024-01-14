---
title:    "C++: 「現在の日付を取得する」"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜ

日付を取得することに関心を持つ理由はさまざまです。中には、オンラインゲームでプレイヤーの登録日を確認するためや、ログを記録する際にタイムスタンプを付けるために日付を取得する必要があるかもしれません。また、特定の作業を自動化するために日付を取得することもあります。どのような理由であっても、日付を取得する方法を知ることは重要です。

## 使い方

C++では、日付を取得するための様々な方法があります。例えば、`<ctime>`ライブラリの`time()`関数を使用する方法や、`<chrono>`ライブラリの`system_clock::now()`関数を使用する方法があります。以下に、それぞれの方法のコード例と出力結果を示します。

```C++
#include <ctime>
#include <iostream>

int main() {
    // time()関数を使用した場合
    time_t currentTime = time(NULL);
    std::cout << "time(): " << ctime(&currentTime) << std::endl;

    // system_clock::now()関数を使用した場合
    auto now = std::chrono::system_clock::now();
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    std::cout << "system_clock::now(): " << std::ctime(&now_c) << std::endl;
}
```

出力結果:

```
time(): Sun Aug 23 15:37:33 2020
system_clock::now(): Sun Aug 23 15:37:33 2020
```

## 詳細を深く

日付を取得する方法には様々な種類がありますが、`<ctime>`ライブラリの`time()`関数を使用する方法が最も一般的です。この関数は、1970年1月1日を基準とした経過秒数を返します。しかし、この値はプログラムを実行するたびに変わるため、ログを記録する場合などはあまり適切ではありません。

そのため、`<chrono>`ライブラリの`system_clock::now()`関数を使用する方がより便利です。この関数は、コンピューターのローカルタイムを返します。また、より精度が高い`<chrono>`ライブラリの`steady_clock`や`high_resolution_clock`を使用することで、より正確な時間を取得することができます。

## 関連情報を参照

- [ctimeライブラリ](https://cpprefjp.github.io/reference/ctime.html)
- [chronoライブラリ](https://cpprefjp.github.io/reference/chrono.html)

## 参考リンク

- [C++で日付と時刻を取得する方法まとめ](https://www.sejuku.net/blog/35775)
- [C++の基本（時刻編）](https://qiita.com/ReedMarchant/items/6f6c74b4c879960e866f)
- [C++で時刻を扱う<chrono>ライブラリ基本型](https://qiita.com/ReedMarchant/items/68e559eb1a470e8f4e59)