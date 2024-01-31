---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSONはデータ交換のフォーマット。シンプルで軽量、多言語対応が理由でプログラマは使う。

## How to:
KotlinでJSONを扱う一例を見ていこう。Kotlinx.serializationライブラリを使う方法です。

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // シリアライズ
    val user = User("Taro", 25)
    val jsonString = Json.encodeToString(User.serializer(), user)
    println(jsonString) // {"name":"Taro","age":25}

    // デシリアライズ
    val userObj = Json.decodeFromString(User.serializer(), jsonString)
    println(userObj) // User(name=Taro, age=25)
}
```

## Deep Dive
JSONはJavaScript Object Notationの略。1999年に登場。XMLより読みやすくて軽い。
代わりにYAMLやTOMLもあるが、ウェブAPIではJSONが主流。KotlinではGson, Moshiなど他にもライブラリが存在する。Kotlinx.serializationは公式対応ライブラリで、コンパイル時のシリアライズコード自動生成が特徴。

## See Also
- Kotlinx.serializationドキュメント： [Kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- JSON 公式: [JSON.org](https://www.json.org/json-ja.html)
- Gsonライブラリ: [Gson on GitHub](https://github.com/google/gson)
- Moshiライブラリ: [Moshi on GitHub](https://github.com/square/moshi)
