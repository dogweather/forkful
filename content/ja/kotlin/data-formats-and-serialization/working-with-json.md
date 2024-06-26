---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:43.379800-07:00
description: "\u65B9\u6CD5\uFF1A Kotlin\u306FJSON\u306E\u305F\u3081\u306E\u7D44\u307F\
  \u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\u3092\u542B\u307E\u306A\u3044\u304C\u3001Google\u306E\
  `Gson`\u3084JetBrains\u306E`Kotlinx.serialization`\u306E\u3088\u3046\u306A\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u5F37\u529B\
  \u306A\u6A5F\u80FD\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  JSON\u3092\u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3089\u3092\u3069\u306E\u3088\
  \u3046\u306B\u4F7F\u7528\u3059\u308B\u304B\u3067\u3059\u3002"
lastmod: '2024-04-05T22:38:41.638926-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Kotlin\u306FJSON\u306E\u305F\u3081\u306E\u7D44\u307F\u8FBC\
  \u307F\u30B5\u30DD\u30FC\u30C8\u3092\u542B\u307E\u306A\u3044\u304C\u3001Google\u306E\
  `Gson`\u3084JetBrains\u306E`Kotlinx.serialization`\u306E\u3088\u3046\u306A\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u5F37\u529B\
  \u306A\u6A5F\u80FD\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  JSON\u3092\u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3089\u3092\u3069\u306E\u3088\
  \u3046\u306B\u4F7F\u7528\u3059\u308B\u304B\u3067\u3059\u3002"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 方法：
KotlinはJSONのための組み込みサポートを含まないが、Googleの`Gson`やJetBrainsの`Kotlinx.serialization`のようなサードパーティライブラリの強力な機能を利用します。以下は、JSONを扱うためにこれらをどのように使用するかです。

### Gsonの使用
`build.gradle`ファイルにGson依存性を追加します：
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

JSON文字列をオブジェクトにパースし、その逆も同様に行います：
```kotlin
import com.google.gson.Gson

// データクラスを定義
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // シリアライズ
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // 出力: {"name":"John Doe","age":30}

    // デシリアライズ
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // 出力: User(name=John Doe, age=30)
}
```

### Kotlinx.serializationの使用
まず、`build.gradle`に依存性を含めます：
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

その後、ビルドスクリプトの先頭で`kotlinx-serialization`プラグインを適用します：
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Kotlinx.serializationを使用したシリアライズとデシリアライズ：
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// シリアライズ可能なデータクラスを定義
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // シリアライズ
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // 出力: {"name":"Jane Doe","age":28}

    // デシリアライズ
    val user = Json.decodeFromString<User>(json)
    println(user)  // 出力: User(name=Jane Doe, age=28)
}
```

GsonとKotlinx.serializationの両方は、KotlinアプリケーションでのJSONの取り扱いを簡略化しますが、一方を他方より選ぶことは、特定のプロジェクト要件や個人の好みに依存します。
