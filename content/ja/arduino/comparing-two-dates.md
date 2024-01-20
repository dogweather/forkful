---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何と何の比較？なぜそれが必要？
日付の比較とは、2つの特定の日時（年、月、日、時、分、秒）を比較することです。プログラマーは、タスクスケジューリングやデータベース管理などで時間の整合性を確保するために日付の比較を行います。

## 実際の方法：
Arduinoで日付を比較するコード例を示します。

```Arduino
#include <TimeLib.h>

time_t t1;
time_t t2;

void setup() {
  setTime(15, 38, 0, 1, 1, 2020); // 設定した日付と時刻
  t1 = now();
  delay(2000); // 2秒遅延
  t2 = now();
  if(t1 < t2){
    Serial.begin(9600);
    Serial.println("t1 is earlier than t2");
  }
}

void loop() {
}
```

このコードは、t1が設定した時刻でt2がその2秒後を意味します。出力は次の通りです：

```
t1 is earlier than t2
```

## ディープダイブ：
日付の比較は古くからあり、非常に多くのプログラミング言語で使用できます。その歴史は、コンピュータサイエンスと同じくらい古いのです。

Arduinoでは、最も一般的な日付比較の方法は上記のようなUnixタイムスタンプを用いた比較です。 それだけでなく、特定のライブラリを用いても日付の比較が可能です。

詳細については、公式のArduinoリファレンスを参照してください。そこでは、タイムスタンプの扱い方だけでなく、TimeLibライブラリ自体の詳細な使用法も掲載しています。

## 参考文献：
1. [Arduino - Time Library](https://playground.arduino.cc/Code/Time/)
2. [Arduino - Time Comparison](https://arduino.stackexchange.com/questions/14125/time-comparison)
これらのリンクはArduinoで日付の比較を深く学ぶための良い出発点でしょう。更に深い理解や応用例を求めるならば、公式ドキュメンテーションや関連するフォーラム、ブログ記事を探してみてください。