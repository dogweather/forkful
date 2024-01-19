---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なんで？どうして？
文字列を小文字に変換するとは、文字列中の全ての大文字を対応する小文字に変えるプログラムの動きのことを指します。これはプログラマーがユーザー入力の一貫性を保つため、またはデータ比較時にケースセンシティブな問題を回避するために行います。

## どうやって：
まずは基本的な構文から見ていきましょう。以下のコードは「Hello,Arduino!」という文字列を小文字に変換します。

```Arduino
#include <Arduino.h>

void setup() {
  String message="Hello,Arduino!";
  message.toLowerCase();
  Serial.begin(115200);
  Serial.print(message);
}

void loop() {

}
```
このコードブロックを実行すると、Serial Monitorで「hello,arduino!」を確認することが出来ます。

## より深く理解する
文字列の小文字化はArduinoプログラミングの初期から存在しました。元々は、ユーザー入力の変動や多様性を吸収するために用いられてきました。他の方法としては、まず各文字がアルファベットの大文字であるかを確認し、そうであれば小文字に変換するという方法があります。しかしアルファベット以外の文字列の場合にはこの方法は使えません。そこでそのような場合には、Arduinoの内蔵関数`toLowerCase()`が便利です。この関数は文字列全体を一度に小文字化します。

## 他にも知っておきたい：
- Arduinoの公式リファレンスの[String.toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)にはより詳しい情報と更なる例があります。
- 別の観点から学びたい場合は、[文字列を大文字に変換する](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)も参照してみてください。