---
title:                "文字列を小文字に変換"
aliases:
- /ja/arduino/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:37:38.990355-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、大文字を小文字に一括して変更することです。これをプログラマーが行う理由は、入力の一貫性を保ち、大文字と小文字の違いによるエラーを防ぐためです。

## How to: (方法)
```Arduino
void setup() {
  Serial.begin(9600);  // シリアル通信の初期化
  String myString = "HeLLo, ArDuinO!";  // 変換する文字列
  myString.toLowerCase();  // 文字列を小文字に変換
  Serial.println(myString);  // 結果を表示
}

void loop() {
  // ループは使いません
}
```
サンプル出力:
```
hello, arduino!
```

## Deep Dive (深堀り)
歴史的に、大文字と小文字の区別は人間にとって意味がありましたが、コンピューターではしばしば問題を引き起こします。例えば、ユーザー名やメールアドレスなどでの大文字小文字の混在です。代替方法はありますが、`String` オブジェクトの `toLowerCase()` メソッドがArduinoで文字列を変換する最も直接的な方法です。このメソッドは文字列の各文字にASCIIルールを適用し、大文字をそれに対応する小文字に変換します。

## See Also (関連情報)
- Arduino String reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- ASCII table and description: https://www.asciitable.com/
- More about string manipulation: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator

Arduinoの世界では、コードがシンプルで機能的であることが重要です。これらのリソースを使って、文字列の扱い方を更に学んでみてください。
