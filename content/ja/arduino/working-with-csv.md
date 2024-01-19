---
title:                "「csvとの作業」"
html_title:           "Arduino: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## 何となぜ？

CSVは "Comma-Separated Values" の略で、テキストベースのデータ共有形式の一つです。プログラマは、効率的かつ簡単にデータを保存・取得するためにこれを使用します。

## 実践：  

ArduinoでCSVファイルを扱うサンプルコードを以下に示します。

```Arduino
#include <SPI.h>
#include <SD.h>

// SDカードのピン設定
const int chipSelect = 4;

void setup() {
  // シリアル通信の開始
  Serial.begin(9600);

  // SDカードの初期化
  if (!SD.begin(chipSelect)) {
    Serial.println("SDカードの初期化に失敗しました");
    return;
  }
  Serial.println("SDカードの初期化に成功しました");

  // CSV fileの読み込み
  File dataFile = SD.open("data.csv");

  if (dataFile) {
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close();
  }
}

void loop() {
  // nothing happens after setup
}
```
このコードは以下の出力を生成します：

```Arduino
123, "Tokyo", 36.5
456, "Osaka", 35.4
789, "Fukuoka", 33.6
```
## ディープダイブ

CSVは、IBMのFORTRAN（FORmula TRANslation）プログラミング言語にその歴史を遡ります。多くの場合、行はレコードを、各列はフィールドを表現します。しかし、タブ、セミコロン、スペースなどが区切り文字として用いられる場合もあり、"TSV"（Tab-Separated Values）、 "DSV"（Delimiter-Separated Values）と呼ばれる形式も存在します。

ArduinoのCSVライブラリには、CSVデータの読み書きに特化した関数が提供されています。

## 参照

1. Arduino CSVライブラリ：
https://www.arduino.cc/en/Reference/SD

2. SDカードのハンドリングについて：
https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite

3. CSVデータ形式について：
https://ja.wikipedia.org/wiki/Comma-Separated_Values

4. 拡張できる代替ライブラリ：
https://www.arduino.cc/en/Reference/Libraries

これらのリンクは、ArduinoプログラミングにおけるCSVファイルの操作に深く関与しています。詳細を理解することで、さらに応用的なプログラミングが可能となります。