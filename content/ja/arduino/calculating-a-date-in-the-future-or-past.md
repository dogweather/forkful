---
title:                "未来または過去の日付を計算する"
html_title:           "Arduino: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Y udo: なぜArduinoプログラムを使って日付を計算するか

Arduinoプログラムを使って日付を計算することで、未来または過去の特定の日付を簡単に特定することができます。これは、特定の日付に基づいて特定のアクションを実行する必要がある場合や、カレンダーイベントを自動的に更新する必要がある場合に役立ちます。

## H w T: Arduinoプログラムで日付を計算する方法

```Arduino
#include <Time.h> // Timeライブラリをインポート

//現在の日付と未来の日付の計算をする関数
void future_date_calculation() {
  int current_day = day(); // 現在の日付を取得
  int current_month = month(); // 現在の月を取得
  int current_year = year(); // 現在の年を取得

  // 1ヶ月後の日付を計算
  int future_month = current_month + 1;
  int future_year = current_year;
  if (future_month > 12) { // 12ヶ月を超えてしまった場合、翌年になるので年を1つ増やす
    future_month = 1;
    future_year++;
  }

  // 日付を出力
  Serial.print("今日の日付は: ");
  Serial.print(current_day);
  Serial.print("/");
  Serial.print(current_month);
  Serial.print("/");
  Serial.print(current_year);
  Serial.print("です。");
  Serial.print("一ヶ月後の日付は: ");
  Serial.print(current_day);
  Serial.print("/");
  Serial.print(future_month);
  Serial.print("/");
  Serial.print(future_year);
  Serial.print("です。");
}

void setup() {
  Serial.begin(9600); // シリアルモニターを開始
  setTime(12, 00, 00, 01, 01, 2020); // 現在の時刻を設定（必要に応じて変更してください）
}

void loop() {
  future_date_calculation(); // 関数を呼び出して日付を計算
  delay(1000); // 1秒待つ
}
```

上記のコードを実行すると、シリアルモニターに以下のように出力されます。

> 今日の日付は: 1/1/2020です。一ヶ月後の日付は: 1/2/2020です。

## D p D: Arduinoプログラムでの日付の計算の詳細

この記事では、Timeライブラリを使用してArduinoプログラムで日付を計算する方法を紹介しました。Timeライブラリには、日付や時刻を取得、計算、表示するための便利な機能が含まれています。コード内で ```setTime()``` 関数を使用することで、現在の時刻を設定できます。また、```day()```、```month()```、```year()```などの関数を使うことで、現在の日付や月、年を取得することができます。

日付を計算する際は、現在の日付を取得し、必要に応じて月や年を調整してから出力するようにしましょう。また、過去の日付の計算も同様の方法で行うことができます。

## See Also: 同じような記事やリソースを参考にしてみてください

- [Arduino Time Libraryのドキュメンテーション](https://www.arduino.cc/reference/en/libraries/time/)
- [未来の特定の日付を計算する方法 (英語)](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Scheduler)
- [過