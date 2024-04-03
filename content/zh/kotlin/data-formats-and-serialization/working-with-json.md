---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:14.693798-07:00
description: "\u5728 Kotlin \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\
  \u793A\u6CD5\uFF09\u6D89\u53CA\u89E3\u6790\u548C\u751F\u6210 JSON \u6570\u636E\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u65B9\u4FBF\u5730\u5728\u5E94\
  \u7528\u7A0B\u5E8F\u7684\u4E0D\u540C\u5C42\u4E4B\u95F4\u6216\u4E0E Web \u670D\u52A1\
  \u901A\u4FE1\u4EA4\u6362\u6570\u636E\uFF0C\u5F97\u76CA\u4E8E JSON \u7684\u8F7B\u91CF\
  \u7EA7\u548C\u6613\u4E8E\u9605\u8BFB\u7684\u683C\u5F0F\u3002"
lastmod: '2024-03-13T22:44:47.743727-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Kotlin \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\
  \u793A\u6CD5\uFF09\u6D89\u53CA\u89E3\u6790\u548C\u751F\u6210 JSON \u6570\u636E\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u65B9\u4FBF\u5730\u5728\u5E94\
  \u7528\u7A0B\u5E8F\u7684\u4E0D\u540C\u5C42\u4E4B\u95F4\u6216\u4E0E Web \u670D\u52A1\
  \u901A\u4FE1\u4EA4\u6362\u6570\u636E\uFF0C\u5F97\u76CA\u4E8E JSON \u7684\u8F7B\u91CF\
  \u7EA7\u548C\u6613\u4E8E\u9605\u8BFB\u7684\u683C\u5F0F\u3002."
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
