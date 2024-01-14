---
title:                "Arduino: csvの操作について"
simple_title:         "csvの操作について"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

# なぜCSVを使用するのか

CSVは「コンマ区切りデータ」の略であり、コンピュータ上で表形式のデータを扱うための一般的な形式です。Arduinoでは、センサーやアクチュエーターとのデータの受け渡しや、外部からのデータの取得に使用されます。CSVを使用することで、データの整理や処理が容易になります。

# 使い方

CSVファイルを読み込んでデータを取得するには、以下のコードを使用します。

```Arduino
#include <SPI.h>
#include <SD.h>
File csvFile;

void setup() {
  // SDカードを初期化
  if(!SD.begin(4)) {
    return;
  }

  // CSVファイルを開く
  csvFile = SD.open("data.csv");

  // ファイルの最初の行を読み込む
  csvFile.readStringUntil('\n');
}

void loop() {
  // ファイルからデータを読み込む
  String data = csvFile.readStringUntil('\n');

  // カンマで区切られたデータを分割し、配列に格納する
  String values[3];
  int index = 0;
  while(data.length() > 0) {
    index = data.indexOf(',');
    if(index == -1) {
      values[2] = data;
      break;
    }
    values[index] = data.substring(0, index);
    data = data.substring(index + 1);
  }

  // データの処理やセンサーへの反映などを行う

  // 0.5秒待つ
  delay(500);
}
```

上記のコードは、ArduinoでSDカードを使用し、data.csvファイルからデータを取得する例です。ファイルの最初の行は読み飛ばし、2行目以降のデータを読み込みます。カンマ区切りのデータを分割し、配列に格納することで、各データを取り出すことができます。

# より詳しい情報

CSVファイルには、数値や文字だけでなく、日付や時間などの特殊なデータ形式を扱うこともできます。また、ファイルを書き込むことも可能です。詳細な情報は、Arduinoが提供するSD.hライブラリのドキュメンテーションを参照してください。

# 関連リンク

- ArduinoのSDライブラリ: https://www.arduino.cc/en/Reference/SD
- CSVの形式について: https://ja.wikipedia.org/wiki/Comma-separated_values