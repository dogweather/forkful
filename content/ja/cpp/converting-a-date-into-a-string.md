---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する必要性は、さまざまな理由で起こり得ます。例えば、データベースに日付を保存する場合や、日付をテキストメッセージなどの形式で送信する場合などが挙げられます。C ++では、日付を文字列に変換する方法を知っていることは非常に重要です。

## 方法

```C++
#include <iostream>
#include <sstream>
using namespace std;

int main() {
    // 今日の日付を取得
    time_t now = time(0);
    tm* date = localtime(&now);

    // 文字列に変換
    stringstream ss;
    ss << date-> tm_year << "-" << date->tm_mon+1 << "-" << date->tm_mday;

    string date_str = ss.str();
    cout << date_str << endl;
}
```

このコード例では、現在の日付データを取得し、`stringstream`を使用して文字列に変換しています。`tm`構造体を使用して日付の年、月、日を取得し、`stringstream`を使用して必要な形式の文字列にフォーマットします。最後に、`ss.str()`を使用して文字列を取得し、出力します。この例では、今日の日付を"yyyy-mm-dd"の形式で出力されるように設定しています。

## ディープダイブ

`sstream`を使用すると、さまざまなデータ型を文字列に変換することができます。例えば、`double`や`int`などの数値を文字列に変換する方法もあります。また、C++11からは、 `std::to_string()`を使用して数値を簡単に文字列に変換することもできます。`std::to_string()`を使用した例は以下の通りです。

```C++
#include <iostream>
using namespace std;

int main() {
    double price = 9.99;
    string price_str = to_string(price);
    cout << "The price is " << price_str << endl;
}
```

上記の例では、`double`の変数`price`を`to_string()`を使用して文字列に変換し、それを`price_str`変数に代入しています。その後、出力文で価格を文字列として使用しています。

## See Also

- [C++日付と時刻の操作方法](https://ja.cppreference.com/w/cpp/chrono)
- [C++ to_string()の使い方](https://programming-place.net/ppp/contents/cpp/string/014.html)