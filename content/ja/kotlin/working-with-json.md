---
title:                "「JSONを扱う」"
html_title:           "Kotlin: 「JSONを扱う」"
simple_title:         "「JSONを扱う」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## JSONとは？
JSONは、データを表現するための軽量で人間に理解しやすい形式です。プログラマーがJSONを作成する理由は、データを簡単に扱えるようにするためです。JSONは現在、Webアプリケーションやモバイルアプリケーションなど、広い範囲で使用されています。

## 使い方：
以下のように、KotlinでJSONを作成し処理することができます。

```Kotlin
// JSONオブジェクトを作成する
val jsonObj = jsonObject("name" to "John", "age" to 25)

// JSON文字列に変換する
val jsonStr = jsonObj.toString()

// JSON文字列からオブジェクトを作成する
val parsedJson = json.parseJSON(jsonStr)

// フィールドにアクセスする
val name = parsedJson["name"]
val age = parsedJson["age"]

// フィールドの値を変更する
parsedJson["name"] = "Jane"

// フィールドを追加する
parsedJson["city"] = "Tokyo"

// JSON文字列に変換する
val newJsonStr = parsedJson.toString()
```

## 詳細を掘り下げる：
JSONは、JavaScript Object Notationの略であり、JavaScriptでデータを扱うために開発されました。しかし、現在ではJavaScript以外の多くのプログラミング言語でも使用されています。代替として、XMLやCSVなどの別のフォーマットがありますが、より簡単に扱えるJSONが人気です。Kotlinでは、KlaxonやJacksonといったライブラリを使用してJSONを処理することができます。

## 関連情報：
- [Klaxon](https://github.com/cbeust/klaxon)
- [Jackson](https://github.com/FasterXML/jackson)
- [JSON入門](https://www.json.org/json-ja.html)