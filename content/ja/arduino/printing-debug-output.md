---
title:    "Arduino: デバッグ出力の印刷"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# なぜ

デバッグ出力を行う理由は、コード内のエラーを特定し、修正するために非常に役立ちます。また、コードの動作を追跡することもできます。デバッグ出力は、Arduinoプログラミングにおいて非常に重要なツールです。

## ハウツー

デバッグ出力を行うためには、Serialモニターを使用します。まず、```Serial.begin(baud rate);``` をsetup関数内に追加し、```Serial.println()``` を使用してデバッグ情報をSerialモニターに出力します。以下は、コーディング例です。

```Arduino
int sensorValue = 0;

void setup() {
  Serial.begin(9600);
}

void loop() {
  sensorValue = analogRead(A0);
  
  // デバッグ情報を出力
  Serial.println("Sensor Value: ");
  Serial.println(sensorValue);
  
  // 2秒待機
  delay(2000);
}
```

Serial.println() を使用することで、変数や文字列などのデータを出力することができます。Serialモニターには、コード内で出力したデバッグ情報が表示されます。

## ディープダイブ

デバッグ出力は、Arduinoコードの動作を追跡し、エラーを特定するだけでなく、コードを最適化するためにも使用することができます。デバッグ出力の使用により、コード内の値の変化を確認し、不要な動作を排除することができます。

また、Serialモニターのボーレートを変更することで、デバッグ出力の速度を調整することができます。これは、コードのパフォーマンスを改善するために役立ちます。

# 参考リンク

- [Arduino公式サイト](https://www.arduino.cc/)
- [Arduino Serialモニターの使用方法](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Serial.println()の使用方法](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)