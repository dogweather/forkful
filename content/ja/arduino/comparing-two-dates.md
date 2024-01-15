---
title:                "日付の比較"
html_title:           "Arduino: 日付の比較"
simple_title:         "日付の比較"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較することが重要な理由は、日付をより効率的に扱うためです。特定の日付が他の日付よりも前か後ろかを判断することで、より複雑な条件分岐や処理を行うことができます。この記事では、Arduinoを使って日付を比較する方法を解説します。

## 方法

比較する日付を変数に代入し、演算子を使用して2つの日付を比較することができます。例を示します。

```Arduino
int date1 = 20210825;
int date2 = 20210826;

if(date1 < date2) {   //date2がdate1よりも大きいことを比較
  Serial.println("date2はdate1よりも後の日付です。");
} else if(date1 > date2) {  //date2がdate1よりも小さいことを比較
  Serial.println("date2はdate1よりも前の日付です。");
} else {   //date1とdate2が同じ日付であることを比較
  Serial.println("date1とdate2は同じ日付です。");
}
```

上記のコードを実行すると、シリアルモニタに「date2はdate1よりも後の日付です。」と表示されます。

## ディープダイブ

日付を比較するためには、まず日付を数値として扱える形に変換することが重要です。Arduinoでは、`toInt()`関数を使用して数値型の変数に変換することができます。また、`strcmp()`関数を使用することで文字型の変数にも対応することができます。

さらに、日付の比較を行う際には「年月日」の順番で比較することが重要です。例えば、2021年8月25日と2021年8月26日を比較する際には、年を最初に比較し、同じであれば月を、さらに同じであれば日を比較します。

## 他の記事

この記事では、Arduinoを使用して日付を比較する方法を紹介しました。他にもArduinoを使った日付処理について知りたい場合は、以下の記事を参考にしてください。

- https://www.jikkyo.org/2019/09/02/arduino-date-print/
- https://deviceplus.jp/hobby/entry_011/
- https://qiita.com/tanukikeiji/items/bfaefb143f05800bba37