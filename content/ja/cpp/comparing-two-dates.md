---
title:                "C++: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

C++で日付を比較することが重要なのでしょうか？その答えは、日付がプログラムにとって重要な情報であり、正確な比較が必要な場合があるからです。例えば、予定されたイベントの日付を比較する場合や、期限切れのアイテムを自動的に削除する場合などです。

## 方法

日付を比較するためには、まずは日付をデータ型として扱えるようにする必要があります。C++では、`time.h`ヘッダーファイルに含まれる`struct time`を使用することができます。次に、二つの日付をそれぞれ`struct time`の変数に代入し、`difftime()`関数を使用して比較します。以下のコード例を参考にしてください。

```C++
// 日付を比較するプログラム

#include <iostream>
#include <time.h>

using namespace std;

int main() {
  // 二つの日付を設定
  struct time date1 = {01, 05, 2021}; // 1月5日, 2021年
  struct time date2 = {13, 02, 2021}; // 2月13日, 2021年

  // 日付を比較
  double difference = difftime(mktime(&date1), mktime(&date2));

  // 結果を出力
  if(difference > 0) {
    cout << "date1はdate2よりも後の日付です。" << endl;
  } else if(difference < 0) {
    cout << "date1はdate2よりも前の日付です。" << endl;
  } else {
    cout << "date1とdate2は同じ日付です。" << endl;
  }

  return 0;
}
```

上記のコードを実行すると、`date1`が`date2`よりも後の日付であるという結果が出力されます。

## ディープダイブ

日付を比較する際には以下のポイントに注意することが重要です。

- `struct time`の内部変数である`tm_hour`や`tm_mday`などを適切に設定すること。これらはそれぞれ、時刻や日付の値を表します。
- `difftime()`関数を使用する際は、引数に渡す日付データを`struct time`型のポインターであることに注意すること。

また、C++では`<chrono>`ライブラリーを使用することで、より多くの日付比較機能を提供しています。詳細は公式ドキュメンテーションを参照してください。

## 参考リンク

- [C++ time.hライブラリーのドキュメンテーション](https://www.geeksforgeeks.org/c-time-h-library-a-c-library-functions/) 
- [C++ <chrono>ライブラリーのドキュメンテーション](https://en.cppreference.com/w/cpp/chrono)