---
title:                "Arduino: 「二つの日付を比較する」"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜArduinoプログラミングで日付を比較するのか

日付を比較することは、プログラミングで非常に便利なことです。たとえば、スケジュールを管理したり、イベントを追跡したりするのに役立ちます。また、日付の差を計算することにより、期日や締切日を計算するのにも役立ちます。

## 方法

Arduinoプログラミングで日付を比較するには、まず2つの日付を変数に格納する必要があります。ここでは、変数date1とdate2を使用します。

```
Arduino
// 日付の例
int date1 = 20210201;
int date2 = 20210315;

// 日付の比較
if (date1 > date2) {
  Serial.println("date1はdate2よりも後の日付です。");
} else if (date1 < date2) {
  Serial.println("date1はdate2よりも前の日付です。");
} else {
  Serial.println("date1とdate2は同じ日付です。");
}
```

上記のコードでは、2つの日付の比較を行い、その結果に応じてメッセージをシリアルモニターに出力します。このように、日付を比較する際はif文を使用することができます。また、日付の形式を変更するには、使用する日付をint型からstring型に変換する必要があります。

## 深く考える

日付を比較する際に重要なのは、日付のフォーマットです。Arduinoでは、日付をint型やstring型で扱うことができますが、それぞれに適した方法で比較する必要があります。また、時間やタイムゾーンが異なる場合も、正しい比較を行うために変換する必要があります。さらに、閏年や月の日数を考慮する必要もあります。これらの要素をすべて考慮することで、正確な日付の比較が可能になります。

## 参考リンク

- [Arduino 日付と時刻の操作](https://www.arduino.cc/reference/en/language/functions/time/date/)
- [日付と時刻の操作のためのライブラリDateTime](https://www.arduino.cc/en/reference/datetime)
- [Stringとしての日付を比較する方法](https://forum.arduino.cc/index.php?topic=330923.0)

# 参考資料

- [Markdown記法まとめ](https://qiita.com/tbpgr/items/989c6badefff69377da7)