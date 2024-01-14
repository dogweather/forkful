---
title:    "Arduino: デバッグ出力の印刷"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# なぜデバッグ出力を行うのか

デバッグ出力とは、Arduinoプログラミングにおいて、コードの実行中に特定のデータや変数の値を表示することを指します。これにより、コードの動作を確認し、問題を特定することができます。デバッグ出力は、Arduinoプロジェクトの開発やトラブルシューティングにおいて非常に役立つツールです。

## デバッグ出力の方法

デバッグ出力は、Serialモニターを使用して行います。まず、最初にSerial.begin()関数を使用して、シリアル通信の速度を設定します。次に、Serial.println()関数を使用して、デバッグしたい変数や文字列をコンソールに出力します。以下のコードは、"Hello World"というメッセージをシリアルモニターに表示する例です。

```arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println("Hello World");
  delay(1000);
}
```

上記の例では、Serial.begin()関数でシリアル通信の速度を9600ビット/秒に設定し、Serial.println()関数でメッセージをシリアルモニターに表示しています。また、delay()関数を使用して、メッセージを1秒ごとに表示するようにしています。

## デバッグ出力の詳細

デバッグ出力を使用する際には、以下の点に注意する必要があります。

- Serial.begin()関数の速度を、シリアルモニターの設定に合わせる必要があります。
- Serial.println()関数を使用すると、データが改行されて表示されるため、Serial.print()関数を使用すると改行されずに連続して表示されます。
- デバッグで使用する変数やメッセージは、必ずシリアルモニターに表示する前にSerial.println()関数で文字列に変換する必要があります。

また、デバッグ出力を使用することで、シリアル通信の速度が遅くなり、プログラムの動作に影響を与えることがあります。そのため、不要なデバッグ出力はコメントアウトするか、実際の使用時にはSerial.begin()関数を削除することをおすすめします。

# See Also

- [Arduino - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino - Serial Monitor](https://www.arduino.cc/en/Guide/ArduinoSerialMonitor/)
- [Arduino - 基本的なコードの書き方](https://amgwellness.jp/arduino_basic_code/)