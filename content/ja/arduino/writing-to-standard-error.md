---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
simple_title:         "標準エラーへの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
エラーメッセージを標準エラー出力（stderr）に書き込むのは、ログやユーザーへのフィードバックと分けて、プログラムの問題を報告するためです。

## How to:
Arduinoでは標準エラーに直接書き込む機能はありません。シリアル出力を使うのが一般的です。

```Arduino
void setup() {
  Serial.begin(9600); // シリアル通信の初期化
}

void loop() {
  // エラーをシリアルに出力
  Serial.println("ERROR: Something went wrong!");
  delay(1000); // 1秒間隔で繰り返す
}
```

シリアルモニター出力:
```
ERROR: Something went wrong!
ERROR: Something went wrong!
...
```

## Deep Dive
Arduinoには標準エラーストリーム`stderr`がないため、`Serial.print()`や`Serial.println()`で代用します。歴史的には環境が限られていたため、単一のシリアルチャンネルを使っていました。デバッグライブラリや外部ツールを使う方法もあります。

## See Also
- [Arduino 公式サイト](https://www.arduino.cc/)
- [Arduino Serial リファレンス](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [デバッグ技術についてのAdafruitのガイド](https://learn.adafruit.com/adafruit-guide-excellent-soldering/common-problems)

この記事ではArduinoプラットフォームでの標準エラー出力の基本を解説しました。興味があれば上記リンクも調べてみてください。
