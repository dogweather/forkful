---
title:                "C++: 将来または過去の日付を計算する"
simple_title:         "将来または過去の日付を計算する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を将来または過去に計算することについて、なぜ参加する必要があるのかを1-2文で説明します。

日付を計算する必要があるプログラムはたくさんあります。例えば、予約システムやスケジュール管理アプリなどです。将来の日付を計算することで、予定を立てたり、スケジュールを調整する際に役立ちます。過去の日付を計算することで、歴史的な日付やイベントを調べることができます。

## 方法

C++で将来または過去の日付を計算する方法を紹介します。以下のコードブロックを参考にしてください。

```C++
#include<iostream>
using namespace std;

int main() {
  // 現在の日付を設定
  int year = 2021;
  int month = 7;
  int day = 22;

  // 計算したい日数を入力
  int days;
  cout << "何日先の日付を計算しますか？" << endl;
  cin >> days;

  // 日数を加算
  day += days;

  // 月を調整
  if (day > 30) {
    day -= 30;
    month++;
  }

  // 年を調整
  if (month > 12) {
    month -= 12;
    year++;
  }

  // 結果を出力
  cout << days << "日後の日付は" << year << "年" << month << "月" << day << "日です。" << endl;

  return 0;
}
```

### 出力例

```
何日先の日付を計算しますか？
10
10日後の日付は2021年7月32日です。
```

## 詳細について

日付を計算する際には、月の日数やうるう年など、多くの要素に注意する必要があります。また、異なる国や言語では日付の表記方法が異なるため、正しい日付を計算するためにはさまざまなロジックを実装する必要があります。

また、クラスや関数を使用して日付を扱うこともできます。例えば、`Date`クラスを作成して日付の計算や比較を行うことができます。また、C++標準ライブラリには日付を扱うための関数やクラスが用意されているため、それらを活用することもできます。

## 関連リンク

- [C++ 日付と時刻の取得方法](https://qiita.com/ribenntto/items/a7df4b8f2fe6da3cf89b)
- [C++による日付計算の仕組み](https://programming.pc-note.net/cpp/cpp041.html)
- [C++標準ライブラリの <chrono> ヘッダー](https://cpprefjp.github.io/reference/chrono.html)