---
title:                "「jsonとの作業」"
html_title:           "Arduino: 「jsonとの作業」"
simple_title:         "「jsonとの作業」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## なにそれ？
JSONとは、データを構造化するためのフォーマットの一種です。プログラマーがJSONを使用する理由は、コンピューターがやりとりするデータを簡単に扱えるようにするためです。

## 使い方：
ArduinoでJSONを扱う方法を説明します。以下のコードブロックを使用して、コーディングの例とサンプル出力を示します。

```Arduino
#include <ArduinoJson.h> // ArduinoJsonライブラリをインポート

// 変数を定義する
int sensorReading = 50;

// JSONオブジェクトを作成する
StaticJsonDocument<200> doc;

// オブジェクトに変数を追加する
doc["sensor"] = sensorReading;

// JSONオブジェクトをシリアルモニターに出力する
serializeJson(doc, Serial);
```

サンプル出力は以下のようになります：

```Arduino
{"sensor":50}
```

## 奥の深さ：
JSONは、1999年にダグラス・クロックフォードによって作成されました。JSONの代替としては、XMLやCSVがありますが、JSONはよりシンプルかつ読みやすい構文を持っています。JSONの実装には、ArduinoJsonライブラリがあります。

## さらに検討する：
以下のリンクから、JSONに関するより詳しい情報を見つけることができます。

- ArduinoJsonライブラリのドキュメンテーション：https://arduinojson.org/
- ダグラス・クロックフォードによるJSONの歴史：https://www.json.org/json-en.html