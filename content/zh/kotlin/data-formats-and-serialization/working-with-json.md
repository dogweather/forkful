---
title:                "使用JSON进行编程"
aliases:
- /zh/kotlin/working-with-json.md
date:                  2024-02-03T19:23:14.693798-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？
在 Kotlin 中处理 JSON（JavaScript 对象表示法）涉及解析和生成 JSON 数据。程序员这样做是为了方便地在应用程序的不同层之间或与 Web 服务通信交换数据，得益于 JSON 的轻量级和易于阅读的格式。

## 如何操作：
Kotlin 没有内置对 JSON 的支持，但利用了第三方库的强大功能，如 Google 的 `Gson` 和 JetBrains 的 `Kotlinx.serialization`。以下是如何使用这两者来处理 JSON。

### 使用 Gson
添加 Gson 依赖到你的 `build.gradle` 文件中：
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

将 JSON 字符串解析为对象，反之亦然：
```kotlin
import com.google.gson.Gson

// 定义一个数据类
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // 序列化
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // 输出：{"name":"John Doe","age":30}

    // 反序列化
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // 输出：User(name=John Doe, age=30)
}
```

### 使用 Kotlinx.serialization
首先，将依赖项包含在你的 `build.gradle` 中：
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

之后，在构建脚本的顶部应用 `kotlinx-serialization` 插件：
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

使用 Kotlinx.serialization 进行序列化和反序列化：
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// 定义一个可序列化的数据类
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // 序列化
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // 输出：{"name":"Jane Doe","age":28}

    // 反序列化
    val user = Json.decodeFromString<User>(json)
    println(user)  // 输出：User(name=Jane Doe, age=28)
}
```

Gson 和 Kotlinx.serialization 都简化了在 Kotlin 应用程序中处理 JSON 的工作，选择其中之一取决于你的特定项目需求和个人喜好。
