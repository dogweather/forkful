---
title:    "Arduino: テキストファイルの作成"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜArduinoでテキストファイルを書くか

Arduinoは、電子回路の制御やデバイスの操作を行うことができるプログラミングボードです。そのため、テキストファイルを書くことでさまざまな情報やデータを保存し、後から読み取ることができます。また、外部のデバイスやセンサーとの連携にも役立つでしょう。

## 書き方

テキストファイルを書くためには、```File```ライブラリを使用します。まずは```setup()```関数でファイルを作成し、```loop()```関数で書き込む内容を指定します。以下のコード例を参考にしてみてください。

```Arduino
#include <File.h>

File myFile;

void setup() {
  Serial.begin(9600);
  myFile = SD.open("test.txt", FILE_WRITE); // ファイルを作成
}

void loop() {
  myFile.println("Hello World!"); // ファイルに書き込み
  myFile.println(12345); // 文字列以外のデータも書き込み可能
}
```

上記のコードを実行すると、SDカード内に「test.txt」というファイルが作成され、その中にテキストとして「Hello World!」と「12345」が書き込まれます。

## 深堀り

テキストファイルを書く際には、ファイルを開いた後、```print()```や```println()```を使用してデータを書き込み、最後に```close()```でファイルを閉じる必要があります。また、ファイルに書き込まれるデータはすべてASCII文字でなければいけません。

SDカードが使用できない場合や、ファイルに書き込みができなかった場合は、```if(myFile)```を使用してエラーをチェックすることができます。

## その他情報

テキストファイルを書く以外にも、Arduinoでは```EEPROM```を使用してデータを保存することもできます。また、外部のクラウドサービスと連携することで、データのバックアップや外部からのデータの取得も可能です。

## 関連リンク

- [Arduino File Library Reference (英語)](https://www.arduino.cc/en/Reference/SD)
- [Arduino EEPROM Library Reference (英語)](https://www.arduino.cc/en/Reference/EEPROM)