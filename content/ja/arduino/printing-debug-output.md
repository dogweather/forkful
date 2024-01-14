---
title:                "Arduino: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力を行うのか

デバッグ出力は、コードの動作を確認するために非常に便利です。Arduinoでプログラムを作成する際、何かがうまくいかない場合やコードに不具合がある場合、デバッグ出力を使用して変数の値やコードの実行状況を確認することができます。これにより、問題点を特定し、改善することができます。

## 方法

デバッグ出力を行うには、Serialライブラリを使用します。まず、`setup()`関数でSerialポートを開き、適切なボーレートを設定します。次に、`loop()`関数内で`Serial.println()`または`Serial.print()`を使用して、出力したい変数やメッセージを指定します。

```Arduino
void setup() {
  Serial.begin(9600); //Serialポートを開き、ボーレートを設定する
}

void loop() {
  int sensorValue = analogRead(A0); //A0ピンからアナログ値を読み込む
  Serial.println(sensorValue); //センサー値をシリアルモニターに出力する
  delay(1000); //1秒待機する
}
```

上記の例では、`sensorValue`変数の値を1秒ごとにシリアルモニターに出力しています。

## ディープダイブ

デバッグ出力を行う際には、`println()`と`print()`の違いを理解することが重要です。`println()`は出力の最後に改行を追加し、`print()`は改行を追加しないため、出力の見やすさが異なります。また、`println()`には複数の引数を指定することができ、それらを連結して出力することができます。

さらに、`Serial.print()`と`Serial.println()`には`print()`と`println()`のように文字列や変数を直接指定する他に、`F()`マクロを使った文字列リテラルを指定することができます。これは、プログラムがメモリを節約するために有効な方法です。

## おわりに

今回は、Arduinoでデバッグ出力を行う方法について紹介しました。デバッグ出力はコードの動作を確認するために役立つ重要なツールです。ぜひ、活用してスムーズなコード作成を目指しましょう！

## 関連リンク

- [Arduino 公式サイト](https://www.arduino.cc/)
- [Arduino 日本語リファレンス](https://www.arduino.cc/reference/jp/)
- [Serial.println()の使い方 - Qiita](https://qiita.com/icchi_h/items/38c3176fb652bd8c0b75)