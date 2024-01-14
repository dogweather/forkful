---
title:                "Kotlin: 「JSONを使ったプログラミング」"
simple_title:         "「JSONを使ったプログラミング」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

＃＃ Ｗｈｙ
多くの人々は、JSONを使用することでデータのシリアライズや通信を行うことができ、アプリケーションの開発をより効率的にすることができます。

＃＃ Ｈｏｗ　Ｔｏ
「JavaScript Object Notation」、通称JSONは、データを簡単に扱うことができるフォーマットです。Kotlinでは、Jacksonというライブラリを使用して、JSONを扱うことができます。以下のコードを参考にしてください。

```Kotlin
import com.fasterxml.jackson.module.kotlin.*
fun main(){
    // JSONをオブジェクトに変換する
    val input = """{"name":"太郎","age":20}"""
    val mapper = jacksonObjectMapper()
    val person: Person = mapper.readValue(input)
    println(person.name) // 太郎
    println(person.age) // 20
    
    // オブジェクトをJSONに変換する
    val person2 = Person("花子", 25)
    val output = mapper.writeValueAsString(person2)
    println(output) // {"name":"花子","age":25}
}

data class Person(val name: String, val age: Int)
```

＃＃ Ｄｅｅｐ　Ｄｉｖｅ
JSONを扱う上でよく使用されるメソッドには、オブジェクトをシリアライズする`writeValue()`や、JSONをオブジェクトにデシリアライズする`readValue()`があります。また、`@JsonIgnore`アノテーションを使用することで、オブジェクトのプロパティをJSONに含めないよう設定することもできます。さらに、複雑なJSONのネストを扱う際には、`ObjectMapper`クラスのメソッドを使用することで、より細かな操作が可能です。

＃＃　Ｓｅｅ　Ａｌｓｏ
- [Jackson](https://github.com/FasterXML/jackson)
- [Kotlin Jackson support](https://github.com/FasterXML/jackson-module-kotlin)
- [KotlinでJSONのデータを操作する方法](https://qiita.com/shimizu/items/1ec15fb3ee73298a2481)