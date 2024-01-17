---
title:                "「未来または過去の日付を計算する」"
html_title:           "Arduino: 「未来または過去の日付を計算する」"
simple_title:         "「未来または過去の日付を計算する」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何をするのか & なぜ?: 
日付を未来や過去に計算することは、指定した日数分日付を変更することです。プログラマーはこの機能を使用して、特定の日付を基準にして日付を調整する必要がある場合に使用します。

## 方法:
日付を未来に計算する最も簡単な方法は、```Arduino ... ```コードブロック内で日付を指定し、指定した日数分だけ加算することです。

```
Arduino DateTime futureDate(2020, 8, 15); //将来の日付を指定
futureDate = futureDate + 365; // 365日後の日付を計算
Serial.println(futureDate); // "2021/8/15" という出力が得られる
```

日付を過去に計算するには、同じように日付を指定し、指定した日数分だけ減算することができます。

```
Arduino DateTime pastDate(2020, 8, 15); //過去の日付を指定
pastDate = pastDate - 365; // 365日前の日付を計算
Serial.println(pastDate); // "2019/8/15" という出力が得られる
```

## さらに詳しく:
日付を計算する方法は複数ありますが、ArduinoのDateTimeライブラリは、日付および時刻の処理に便利です。また、現在のバージョンのArduinoでは、Timeライブラリを使用することもできます。

また、日付を計算する代替手段として、プログラミング言語に組み込まれた日付関数や、カレンダーライブラリを使用する方法があります。

具体的な実装方法については、Arduinoの公式ドキュメントやフォーラムで情報を収集することができます。

## 関連リンク:
- [Arduino DateTimeライブラリ](https://www.arduino.cc/en/Reference/DateTimeConstructor)
- [Arduino Timeライブラリ](https://www.arduino.cc/en/Reference/Time)
- [プログラミングにおける日付処理の基本](https://dev.classmethod.jp/articles/date-processing-in-programming/)
- [日付関数の使い方](https://www.sejuku.net/blog/100113)
- [カレンダーライブラリの使い方](https://circuitdigest.com/microcontroller-projects/arduino-calendar-real-time-clock-with-ds1307-and-arduino)