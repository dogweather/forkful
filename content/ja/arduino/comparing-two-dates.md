---
title:    "Arduino: 「日付の比較」"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ比較するのか

日々の生活では、時刻や日付を比較する必要があります。例えば、過去のイベントと現在のイベントを比較したり、予定を立てるために時間の差を計算したりする必要があります。このようなシナリオでは、日付を正しく比較することが重要です。そこで、今回はArduinoプログラミングにおける日付比較の方法について説明します。

## 日付比較の方法

日付を比較するには、DateTimeライブラリを使用します。DateTimeライブラリには、日時を表現するためのDateTimeオブジェクトや、日付の演算を行うための演算子が用意されています。まずは、DateTimeオブジェクトを使って現在の日付を取得する方法から始めましょう。

```Arduino
#include <RTClib.h> 
RTC_DS3231 rtc; 

void setup () { 
  if (! rtc.begin()) { 
    Serial.println("Could not find RTC"); 
    while (1); 
  }
  DateTime now = rtc.now();
  Serial.print(now.year());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.println(now.day());
}
```

このコードを実行すると、現在の日付がシリアルモニタに表示されます。次に、過去の日付を取得して比較する方法を見ていきましょう。

```Arduino
// 過去の日付を設定する例
DateTime past(2021, 2, 5, 10, 0, 0);

// 比較演算子を使って日付を比較する
if(now < past) {
  // 現在の日付が過去の日付よりも前であることを示すメッセージを表示する
  Serial.println("今日は過去の日付です");
}
```

さらに、日付の差を計算する方法もあります。例えば、現在の日付と過去の日付の差を「日」で計算する場合は、次のように記述します。

```Arduino
int difference = now.daysBetween(past);
Serial.print("過去から現在までの日数: ");
Serial.println(difference);
```

これで、日付の比較や差の計算ができるようになりました。

## 深く掘り下げる

DateTimeライブラリには、実はもっと多くの機能があります。例えば、日付の書式を変更する方法や、指定した日付がうるう年かどうかを判定する方法などがあります。さらに、RTCモジュールを使用して日付を取得する方法もあります。興味がある方は、公式ドキュメントを参考にしてみてください。

## 関連記事

- [DateTimeライブラリの公式ドキュメント](https://www.arduino.cc/en/Reference/DateTime)
- [RTCモジュールを使用する方法について学ぶ](https://maker.pro/arduino/projects/how-to-use-an-rtc-module-to-control-your-arduino)
- [Arduinoを使った日付の差の計算方法](https://www.instructables.com/DateTime-Library-Usage/)