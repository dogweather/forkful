---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の大文字化とは、文字列内の全ての小文字を大文字に変換することです。プログラマーは可読性を高めたり、固有名詞やアクロニムを表現するためによく使います。

## How to: (方法)
```Arduino
void setup() {
  Serial.begin(9600); // シリアル通信の開始
  String message = "こんにちは、Arduino!";
  Serial.println(capitalizeString(message)); // 大文字化した文字列を出力
}

void loop() {
  // このサンプルではloop内は不要です。
}

String capitalizeString(String input) {
  input.toUpperCase(); // 文字列を全て大文字に
  return input;
}

/* 出力:
 KONNICHIHA、ARDUINO!
*/
```

## Deep Dive (掘り下げ)
文字列の大文字化は初期のコンピュータ・システムにおける制限から発展しました。古いシステムは大文字のみをサポートしていたため、大文字がデフォルトになりました。書式設定オプションとして使われるようになった今も、慣例や強調のために用いられます。

選択肢としては`toUpperCase()`関数を使う他に、`for`ループで各文字を大文字に変換する手動の方法もあります。しかし、`toUpperCase()`は効率的でエラーが少なく、Arduino において最も一般的な方法です。

実装については、`toUpperCase()`メソッドは内部でASCII値を操作して大文字への変換を効率的に行っている点が重要です。Unicode文字に対応する場合は別途ライブラリや追加のコードが必要になることもあります。

## See Also (参照)
- Arduinoの公式リファレンスで`String`クラスを更に学ぶ: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- 文字コードについて詳しく知る: [ASCII Table and Description](https://www.asciitable.com/)
