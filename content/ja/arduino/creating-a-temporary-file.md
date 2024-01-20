---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ? / What & Why?
一時的なファイルの作成とは、データの一時的な保存/変更を行うためのものです。プログラマーがこれを行う主な理由は、一時的な操作を行うためやデータのバックアップを取るためです。

## 方法 / How to:
残念ながら、Arduinoには一時ファイルを直接作成する機能は提供されていません。代わりに、SDカードを使用してこの機能を再現することができます。以下にそのコード例と出力例を示します。

```Arduino
#include <SD.h>

File tmp;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ;
  }

  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    while (1);
  }
  tmp = SD.open("temp.txt", FILE_WRITE);
}

void loop() {
  if (tmp) {
    tmp.println("Temp data");
    tmp.close();
    Serial.println("Temp data written to file");
  } else {
    Serial.println("Error opening file");
  }
}
```
起動時に"temp.txt"という一時ファイルが作成され、そのファイルに"Temp data"というデータが書き込まれます。一時ファイルにデータが書き込まれたことを確認するために、シリアルモニタにも出力が行われます。

## ディープダイブ / Deep Dive
Arduinoの歴史的背景を振り返ると、一時ファイルの作成などの複雑なファイル操作は、そのメモリ制約と情報処理能力の制約のために、最初からサポートされていませんでした。しかし、SDカードを使用することにより、この問題をある程度緩和することが可能です。

代替方法としては、EEPROMやSPIFFSなど他の不揮発性メモリを利用する方法がありますが、それらはハードウェアの制約や寿命などの問題が存在します。

一時ファイルの実装について言えば、実際にはファイルを作成したり消去したりするのではなく、単にファイルに書き込みを行ったり、読み込みを行ったりするだけです。

## 参考資料 / See Also
こちらの関連記事も参考になるかもしれません：
1. ArduinoのSDライブラリ：<https://www.arduino.cc/en/Reference/SD>
2. EEPROMの使用法：<https://www.arduino.cc/en/Tutorial/libraryExamples/EEPROMWrite>
3. SPIFFSの解説：<https://randomnerdtutorials.com/esp8266-nodemcu-spiffs-arduino/>