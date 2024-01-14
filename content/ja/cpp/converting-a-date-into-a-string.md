---
title:    "C++: 日付を文字列に変換する"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜ

プログラミングをする際、日付を文字列に変換する必要があることがあります。日付を好きな形式で表現するためや、データベースに格納する際に、文字列に変換することが多いです。

## 方法

まずは、日付を文字列に変換するためのC++のコードをご紹介しましょう。

```C++
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>

using namespace std;

int main() {
    // 変換したい日付を設定
    int day = 17;
    int month = 8;
    int year = 2021;

    // 文字列に変換するためのストリームを作成
    stringstream ss;

    // ストリームに日付を書き込み
    ss << setfill('0') << setw(2) << day << "/" << setw(2) << month << "/" << year;

    // 文字列に変換
    string date_string = ss.str();

    // 変換した文字列を出力
    cout << date_string << endl;

    return 0;
}
```

上記のようなコードを実行すると、`"17/08/2021"`という文字列が出力されます。

また、`"%d/%m/%Y"`といったフォーマットを指定することで、より詳細な日付の表現が可能です。詳しくはC++の日付フォーマット指定について調べてみてください。

## ディープダイブ

日付を文字列に変換する際には、グレゴリオ暦が採用されていることに注意が必要です。また、ローカルな設定によって日付の表現が異なることもあります。そのため、プログラムを書く際にはこれらを考慮し、適切なフォーマットを選択することが重要です。

## さらに参考になる情報

- [C++ 日付フォーマット指定](https://www.cplusplus.com/reference/ctime/strftime/)
- [日付の文字列変換についてのStack Overflowの質問](https://stackoverflow.com/questions/20609132/converting-an-int-to-a-date-string)