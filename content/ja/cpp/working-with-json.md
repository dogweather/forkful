---
title:                "jsonとの作業"
html_title:           "C++: jsonとの作業"
simple_title:         "jsonとの作業"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを使うのか

JSONはJavaScript Object Notationの略称で、データをシンプルかつ柔軟に格納するためのフォーマットです。C++では簡単にJSONデータを取り扱うことができ、APIからのデータの受け取りやネットワーク通信におけるデータの送受信などに活用することができます。

## JSONの使い方

まずは、JSONを使うために必要なライブラリをインストールする必要があります。C++の場合、[RapidJSON](https://github.com/Tencent/rapidjson/)や[JSON for Modern C++](https://github.com/nlohmann/json)などのライブラリを利用することができます。

次に、JSONデータを扱うための基本的なコードを紹介します。

```C++
#include <iostream>
#include "rapidjson/document.h"
#include "rapidjson/prettywriter.h"

using namespace rapidjson;
using namespace std;

int main() {
    // JSONデータを定義
    const char* data = "{\"name\": \"Taro\", \"age\": 30, \"city\": \"Tokyo\"}";

    // JSONパーサーのオブジェクトを作成
    Document document;

    // JSONデータをパースし、オブジェクトに格納
    document.Parse(data);

    // JSONデータの取得
    cout << "Name: " << document["name"].GetString() << endl;  // Taro
    cout << "Age: " << document["age"].GetInt() << endl;  // 30
    cout << "City: " << document["city"].GetString() << endl;  // Tokyo

    // JSONデータの編集
    document["age"].SetInt(35);

    // 編集したJSONデータを出力
    StringBuffer buffer;
    PrettyWriter<StringBuffer> writer(buffer);
    document.Accept(writer);
    cout << "Updated data: " << buffer.GetString() << endl;  // {"name": "Taro", "age": 35, "city": "Tokyo"}

    return 0;
}
```

以上のように、JSONライブラリを利用することで、簡単にJSONデータを扱うことができます。また、チュートリアルやサンプルコードを参考にして、さまざまなJSONデータを取り扱ってみることもおすすめです。

## JSONの深堀り

今回紹介した詳細のコードを見るとわかるように、JSONはオブジェクトと配列を組み合わせることで、複雑なデータ構造を表現することができます。また、JSONはテキストベースのフォーマットなので、人が読み書きすることが容易で、データの可読性が高いのが特徴です。さらに、JSONは多くのプログラミング言語でサポートされており、データの相互変換も簡単に行うことができます。

## 他にも参考になるリンク

- [RapidJSON公式ドキュメント](http://rapidjson.org/)
- [JSON for Modern C++公式ドキュメント](https://github.com/nlohmann/json/blob/develop/README.md)
- [JSONとは？- MDN Web Docs](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Objects/JSON)