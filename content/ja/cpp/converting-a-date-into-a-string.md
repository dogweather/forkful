---
title:    "C++: 日付を文字列に変換する"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

C++を勉強する際、日付を文字列に変換する必要があることがあります。そのため、この記事では日付の文字列変換について説明します。

## 使い方

日付を文字列に変換するには、次のようなコードを使用します。

```C++
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main() {
    // 現在の日付を取得
    time_t now = time(nullptr);

    // 文字列に変換
    string date_string = ctime(&now);

    // 出力
    cout << "今日の日付は" << date_string << "です。" << endl;

    return 0;
}
```

このコードを実行すると、次のような出力が得られます。

```
今日の日付はFri Jul 2 20:20:21 2021です。
```

このように、`ctime`関数を使用することで日付を文字列に変換することができます。

## 深堀り

日付を文字列に変換する際には、`<ctime>`ヘッダーファイルを使用する必要があります。また、`time_t`というデータ型を使用して現在の日付を取得します。`ctime`関数は、`time_t`型の変数を引数として受け取り、`char`型の文字列を返します。これにより、日付を必要な形式の文字列に変換することができます。

## 関連リンク

- [C++ 日付と時刻](https://programming-place.net/ppp/contents/cpp/language/006.html)
- [std::time_t](https://cpprefjp.github.io/reference/ctime/time_t.html)
- [std::string](https://cpprefjp.github.io/reference/string/string.html)
- [std::ctime](https://cpprefjp.github.io/reference/ctime/ctime.html)