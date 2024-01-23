---
title:                "JSONを扱う方法"
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JSONはデータ交換のフォーマット。シンプルで軽量。プログラマーはデバイス間通信やWeb API利用のために使う。

## How to: (方法)
ArduinoJsonライブラリを用いた例。インストール必要。
```c++
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  // JSONを使ってデータを作成
  StaticJsonDocument<200> doc;
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;

  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  // JSONをシリアルに出力
  serializeJson(doc, Serial);
}

void loop() {
  // ここに何もしない
}

```
出力例：
```json
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

## Deep Dive (深掘り)
JSONとはJavaScript Object Notationの略。2001年に登場。XMLより軽量で読み書きしやすい。Arduinoでは、ArduinoJsonライブラリが広く使われている。このライブラリは動的・静的メモリ・アロケーションをサポートし、ストリーム入出力可能。

## See Also (関連情報)
- ArduinoJson公式ドキュメント: https://arduinojson.org/
- JSONの基本知識: https://www.json.org/json-ja.html
- Arduinoのリファレンス: https://www.arduino.cc/reference/jp/
