---
title:                "Arduino: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ日付を比較する必要があるのか？

Arduinoプログラムを書くとき、時には異なる日付を比較する必要があります。例えば、イベントの日付をチェックして実行するコードを書く場合や、センサーで取得した日付と現在の日付を比較してデータを処理する場合などが挙げられます。

## 日付の比較方法

日付を比較するためには、まずは日付データを取得する必要があります。Arduinoでは、 `Y`(年), `m`(月), `d`(日)の3つの変数を使って現在の日付を取得することができます。

例えば、以下のように記述することで、現在の年月日の値を取得することができます。

```arduino
int year = Y; // 現在の年
int month = m; // 現在の月
int day = d; // 現在の日
```

次に、比較したい日付を変数に代入します。例えば、以下のように記述することで、比較したい日付を設定することができます。

```arduino
int compare_year = 2020; // 比較したい年
int compare_month = 12; // 比較したい月
int compare_day = 25; // 比較したい日
```

そして、`if`文を使って日付の比較を行います。例えば、以下のように記述することで、現在の日付と比較したい日付が一致しているかどうかをチェックすることができます。

```arduino
if (year == compare_year && month == compare_month && day == compare_day) {
  // 日付が一致した場合の処理
} else {
  // 日付が一致しなかった場合の処理
}
```

このように、`if`文を使って日付の一致をチェックすることができます。

## 日付の比較のさらなる学び

今回紹介した方法は日付を比較する際の基礎的な方法ですが、実際のプロジェクトではより複雑な日付の比較が必要になる場合もあります。例えば、うるう年の扱いや、時分秒の情報も含めた比較などがあります。

より詳しい情報を知りたい方は、以下のリンクを参考にしてみてください。

## 関連情報を見る

- [Arduino Reference - Time](https://www.arduino.cc/reference/en/libraries/time/)
- [Arduino Time Library](https://github.com/PaulStoffregen/Time)
- [Arduinoで日付の処理をする方法](https://deviceplus.jp/hobby/entry007/)