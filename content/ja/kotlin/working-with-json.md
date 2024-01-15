---
title:                "JSONを使用する"
html_title:           "Kotlin: JSONを使用する"
simple_title:         "JSONを使用する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを使用するか

今やWeb開発において、データの転送形式としてJSONは欠かせないものとなりました。KotlinとJSONを組み合わせることで、よりスマートなコーディングが可能になります。

## KotlinでJSONを扱う方法

Kotlinでは、GoogleのGSONライブラリを使用することで簡単にJSONを扱うことができます。以下のコードは、JSON形式のデータをKotlinオブジェクトに変換する例です。

```
import com.google.gson.Gson

fun main() {
    val json = """{"name":"John","age":27}"""
    val person = Gson().fromJson(json, Person::class.java)
    println(person.name)
}
```

出力結果は"John"となります。このように、KotlinではGSONを使用することでJSONデータを扱うことができます。

## JSONを深く掘り下げる

GSONライブラリを使用することで、より複雑なJSONデータを扱うことができます。また、Kotlinの拡張機能を使用することで、よりスマートなコーディングが可能になります。以下のコードは、リストの中にオブジェクトが含まれるJSONデータをKotlinオブジェクトに変換し、各オブジェクトのnameプロパティを出力する例です。

```
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken

fun main() {
    val json = """[{"name":"John"},{"name":"Sarah"}]"""
    val listType = object : TypeToken<List<Person>>() {}.type
    val personList = Gson().fromJson<List<Person>>(json, listType)
    for (person in personList) {
        println(person.name)
    }
}

data class Person(val name: String)
```

出力結果は、"John"と"Sarah"となります。このように、KotlinとGSONを組み合わせることで、より柔軟にJSONデータを扱うことができます。

## 関連リンク

- [GSON公式ドキュメント](https://github.com/google/gson)
- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/home.html)
- [JSONパースライブラリの比較](https://www.baeldung.com/java-performance-json-libraries)