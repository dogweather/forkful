---
title:                "Arduino: 「jsonを使ったプログラミング」"
simple_title:         "「jsonを使ったプログラミング」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを学ぶのか

JSON(JavaScript Object Notation)は、様々なプログラムやアプリケーションでデータをやり取りするためのフォーマットです。Arduinoでのプログラミングを行う際に、JSONを扱うことでより高度なデータ処理を行うことができます。

## JSONの使い方

以下のような形式でArduinoでJSONを使用することができます。

```
ArduinoJSON ライブラリをインストールする必要があります。

#include <ArduinoJson.h>

DynamicJsonDocument doc(1024);//メモリサイズは適宜変えてください

String jsonStr = "{\"name\":\"Taro\", \"age\":20}";
DeserializationError err = deserializeJson(doc,jsonStr);

if(err){
  //エラー処理
}

String name = doc["name"]; //"Taro"
int age = doc["age"]; //20

```

JSONデータをパースするために、ArduinoJSONライブラリをインストールする必要があります。最初に使用する際には、メモリサイズを適宜変更する必要があります。上記の例では、JSONデータが文字列として定義されているため、それをパースしてdocオブジェクトに格納し、データを取り出しています。

## JSONの詳細

JSONは名前と値のペアの集合で構成されており、そのデータは階層構造を持つことができます。つまり、名前と値のペアが更に名前と値のペアを持つことができるということです。これにより、複雑なデータも簡単に扱うことができます。また、ArduinoJSONライブラリは、JSONデータをパースする際にメモリを効率的に使用するように設計されているため、メモリ管理の負担も低く済みます。

## もっと詳しく学ぶには

以下のリンクを参考に、より詳しい内容を学ぶことができます。

- [ArduinoJSONライブラリのドキュメント](https://arduinojson.org/v6/api/jsonobject/)
- [JSONの基礎知識](https://www.json.org/json-ja.html)
- [プログラミング言語JavaScriptのJSON関数について](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/JSON) 

---

## 関連リンク

- [ArduinoJSONライブラリのドキュメント](https://arduinojson.org/v6/api/jsonobject/)
- [JSONの基礎知識](https://www.json.org/json-ja.html)
- [プログラミング言語JavaScriptのJSON関数について](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/JSON)