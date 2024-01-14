---
title:                "Arduino: 一時的なファイルの作成"
simple_title:         "一時的なファイルの作成"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Arduinoで一時ファイルを作成する理由

一時ファイルは、一時的なデータを一時的に保存するのに役立ちます。例えば、センサーからのデータを一時的に保存したり、プログラム実行中の一時的な値を保持したりできます。そうすることで、より効率的にプログラムを実行することができます。

Arduinoで一時ファイルを作成する方法

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile; //一時ファイルを指定する変数を宣言

void setup() {
  //SDカードを初期化
  if (!SD.begin(4)) {
    Serial.println("SDカードを初期化できませんでした");
    while (1);
  }
  Serial.print(F("SDカードの容量: "));
  Serial.println(SD.card()->capacity());
  //一時ファイルをSDカードに作成
  myFile = SD.open("temp.txt", FILE_WRITE); 
  if (myFile) {
    Serial.println("一時ファイルを作成しました");
    //データを書き込む
    myFile.println("データを書き込む");
    myFile.close();
  }
}

void loop() {
  //一時ファイルからデータを読み込む
  myFile = SD.open("temp.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  }
}

```

一時ファイルを作成する際には、まずSDカードを初期化します。その後、```SD.open()```を使用して一時ファイルを作成し、必要なデータを書き込むことができます。一時ファイルを使用する際は、```SD.open()```に``FILE_WRITE``を指定し、ファイルを書き込みモードにする必要があります。また、一時ファイルの読み込みには```SD.open()```のみでよく、```available()```と```read()```を使用してデータを読み込むことができます。

一時ファイルの詳細

一時ファイルを作成する際には、SDカードの初期化やファイルのオープンなど、いくつかの手順が必要です。また、一時ファイルを使用する際には、ファイルを書き込みモードや読み込みモードにすることが重要です。一時ファイルをうまく活用することで、よりスマートなプログラムを作成することができます。

## その他の参考リンク

- [Arduino SDライブラリー](https://www.arduino.cc/en/Reference/SD)
- [SDカードの使い方](http://www.musashinodenpa.com/arduino/ref/index.php?f=9#9_95)
- [ファイルデータの読み書き](https://analog-tyo.com/tips/1741)

## 参考文献

- [Arduino SD library](https://www.arduino.cc/en/Reference/SD)
- [Using the SD library](https://www.arduino.cc/en/Tutorial/ReadWrite)]