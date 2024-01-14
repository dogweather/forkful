---
title:                "C++: 日付を文字列に変換する"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することに取り組む理由を説明します。

## 方法
「```C++ ... ```」コードブロック内にコーディング例とサンプル出力を記載します。
例：
```C++
#include <iostream>
using namespace std;
int main() {
    // 日付を文字列に変換する
    int day = 29;
    int month = 10;
    int year = 2021;
    string date = to_string(month) + "/" + to_string(day) + "/" + to_string(year);
    cout << "変換後の日付：" << date << endl;
    return 0;
}
```
出力：
```
変換後の日付：10/29/2021
```

## ディープダイブ
日付を文字列に変換するときには、**stringstream**クラスを使用することで、より柔軟な方法で変換することができます。stringstreamを使用することで、日付のフォーマットや特定の言語に合わせた文字列に変換することができます。
例えば、上記のコードをstringstreamを使用して書き換えると次のようになります。
```C++
#include <iostream>
#include <sstream>
using namespace std;
int main() {
    // 日付を文字列に変換する
    int day = 29;
    int month = 10;
    int year = 2021;
    stringstream ss;
    ss << year << "年" << month << "月" << day << "日";
    string date;
    ss >> date;
    cout << "変換後の日付：" << date << endl;
    return 0;
}
```
出力：
```
変換後の日付：2021年10月29日
```

## 参考リンク
「See Also」
- [C++ stringstreamクラスの使用方法](https://www.cplusplus.com/reference/sstream/stringstream/)
- [C++ to_string関数の使用方法](https://www.cplusplus.com/reference/string/to_string/)
- [C++ 日付のフォーマット](https://www.cplusplus.com/reference/ctime/strftime/)

**Happy coding!**