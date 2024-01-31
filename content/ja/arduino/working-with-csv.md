---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSVは"Comma-Separated Values"の略で、値がコンマで区切られているファイル形式です。プログラマはデータを簡単に交換・保存するためにCSVを使います。

## How to: (方法)
ArduinoでCSVデータを扱う例を示します。

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  myFile = SD.open("test.csv");
  if (myFile) {
    while (myFile.available()) {
      String data = myFile.readStringUntil('\n');
      Serial.println(data);
    }
    myFile.close();
  } else {
    Serial.println("Error opening file");
  }
}

void loop() {
  // メインコードはここに書きます。
}
```
サンプル出力：
```
sensor1,30,sensor2,60
sensor1,31,sensor2,61
sensor1,32,sensor2,62
```

## Deep Dive (掘り下げ)
CSV形式の歴史は1970年代にさかのぼります。JSONやXMLのような代替手段がありますが、CSVはそのシンプルさから広く採用されています。実装時にはCSVの構造に注意が必要で、例えばデータにコンマが含まれている場合にはデータをダブルクォーテーションで囲む等の対策が必要です。

## See Also (関連情報)
- ArduinoのSDライブラリ: https://www.arduino.cc/en/Reference/SD
- CSVに関するRFC 4180: https://tools.ietf.org/html/rfc4180
- オープンソースのCSVパーサライブラリ: https://github.com/bblanchon/ArduinoJson
