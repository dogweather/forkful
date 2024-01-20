---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

JSON，全称JavaScript Object Notation，是一种数据格式，用于数据交换。程序员使用它因为它轻量、易读写，且易于在网络上传输。

## How to: (如何操作：)

在Kotlin中解析JSON通常使用库如`kotlinx.serialization`。下面是个简单例子：

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class Person(val name: String, val age: Int)

fun main() {
    val json = """{"name": "John", "age": 30}"""
    
    // JSON字符串转换为Person对象
    val person = Json.decodeFromString<Person>(json)
    println(person)

    // Person对象转换回JSON字符串
    val jsonString = Json.encodeToString(person)
    println(jsonString)
}
```
输出：
```
Person(name=John, age=30)
{"name":"John","age":30}
```

## Deep Dive (深入探索)

JSON格式问世于2001年，由Douglas Crockford提出。除了`kotlinx.serialization`，有时开发者也使用`Gson`和`Moshi`等替代库进行JSON处理。在Kotlin中，直接使用这些库可能需要额外的类型转换和模板代码;`kotlinx.serialization`库为Kotlin提供了更原生的支持。

## See Also (另请参阅)

- Kotlin官方文档：[kotlinx.serialization](https://kotlinlang.org/docs/serialization.html)
- GitHub上的`kotlinx.serialization`库：[GitHub kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- JSON 官方网站：[JSON](http://json.org/)