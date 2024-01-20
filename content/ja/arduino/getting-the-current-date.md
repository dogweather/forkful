---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 現在の日付の取得: Arduinoプログラミングガイド

## 何となぜ?

現在の日付の取得は、リアルタイムクロックから現時点の年、月、日を知るためのプロセスです。これにより、プログラマーは時間に依存した関数やタスクを正確に管理し、決定的な結果を提供することができます。

## どうやって:

次の例は、Arduinoを使用した現在の日付の取得の基本的な方法を示しています。RTClib ライブラリーを用いています。

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Wire.begin();
  rtc.begin();

  if (! rtc.isrunning()) {
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop () {
  DateTime now = rtc.now();
  
  Serial.print("現在の日付(YYYY/MM/DD): ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
  delay(1000);
}
```

このコードは現在の日付（年/月/日）を毎秒表示します。

## ディープ・ダイブ

* **歴史的背景**: RTC, またはリアルタイムクロックは、日付と時間を計測して保持することができる特殊なガジェットです。Arduinoは本来RTCを内蔵しておらず、通常は外部モジュールを追加することでRTC機能が提供されます。

* **代替手段**: RTClibではなく、TimeLibやDS3231ライブラリーなど、他のライブラリーを利用することも可能です。それぞれのライブラリーは独自の特性を持っており、プロジェクトの要件に応じて選択できます。

* **実装詳細**:`rtc.now()`関数は現在の日時情報を提供します。これにより、年 (`now.year()`), 月 (`now.month()`), 日 (`now.day()`) の値をそれぞれ取得することが可能です。

## 参考リンク：

1. ライブラリーの詳細：[RTClib公式ドキュメント](https://www.arduino.cc/reference/en/libraries/rtclib/)
2. 別のRTCライブラリー：[DS3231公式ドキュメント](https://www.arduino.cc/reference/en/libraries/ds3231/)
3. サンプルとチュートリアル：[Arduinoプロジェクトハブ](https://create.arduino.cc/projecthub)

実際の使用法と詳細については、これらのリンクが大いに役立つでしょう。普段のアプリケーション開発と同時に、教育、研究、ホビープロジェクトにも役立ててみてください。