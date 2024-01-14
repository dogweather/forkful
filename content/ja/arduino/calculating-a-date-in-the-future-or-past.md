---
title:                "Arduino: 未来や過去の日付を計算する"
simple_title:         "未来や過去の日付を計算する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
誰もが時折、特定の日付を計算する必要があります。たとえば、将来のある日までの経過日数を知る必要があったり、過去のある日からの日数を計算したい場合があります。Arduinoプログラミングを通して、日付の計算ができるようになれば、自動化されたタスクやプロジェクトの開発に役立ちます。

## How To
日付の計算はプログラムを使用する必要があります。今回はArduinoを使用しますが、同じ原則は他のプログラミング言語でも使えるものです。

まずは、指定した日付から過去または未来の日付を計算する方法をご紹介します。以下のコードをArduinoに入力し、実行してみましょう。

```
Arduino ...

# 過去の日付の計算
#include <Time.h>

// 今日の日付を設定
int today = 2019/12/01;

// 過去の日付を計算する関数
void calculateDate(int day, int month, int year) {
  int pastDate = day + 1;
  int pastMonth = month;
  int pastYear = year;

  // 月の計算
  if (day == 31 && month == 1) {
    pastDate = 1;
    pastMonth = 12;
    pastYear--;
  } else if (day == 31 && month == 3) {
    // 閏年の判定
    if (year % 4 == 0) {
      pastMonth--;
      pastDate = 29;
    } else {
      pastMonth--;
      pastDate = 28;
    }
  } else if (day == 1) {
    pastMonth--;
    pastDate = 31;
  } else {
    pastDate--;
  }

  // 出力
  Serial.print("過去の日付は：");
  Serial.print(pastDate);
  Serial.print("/");
  Serial.print(pastMonth);
  Serial.print("/");
  Serial.println(pastYear);
}

void setup() {
  Serial.begin(9600);
  calculateDate(today, 12, today);
  // 今日の日付から1日前の日付を計算
}

void loop() {
  // 無限ループ
}
```

このコードを実行すると、シリアルモニタに過去の日付が表示されるはずです。

同じように、今日の日付から未来の日付を計算するには、以下のコードを入力し、実行します。

```
Arduino ...
// 未来の日付の計算
#include <Time.h>

// 今日の日付を設定
int today = 2019/12/01;

// 未来の日付を計算する関数
void calculateDate(int day, int month, int year) {
  int futureDate = day + 1;
  int futureMonth = month;
  int futureYear = year;

  // 月の計算
  if (day == 28 && month == 2) {
    // 閏年の判定
    if (year % 4 == 0) {
      futureMonth++;
      futureDate = 29;
    }
  } else if (day == 30 && month == 4 || day == 30 && month == 6 || day == 30 && month == 9 || day == 30 && month == 11) {
    futureMonth++;
    futureDate = 1;
  } else if (day == 31 && month == 12) {
    futureYear++;
    futureMonth = 1;
    pastDate = 1;
  } else if (day == 31) {
    futureMonth++;
    futureDate = 1;
  } else {
    futureDate++;
  }

  // 出力
  Serial.print("未来の日付は：");
  Serial.print(futureDate);
  Serial.print("/");
  Serial.print(futureMonth);
  Serial.print("/");
  Serial.println(futureYear);
}

void setup() {
  Serial.begin(9600);
  calculateDate(today,