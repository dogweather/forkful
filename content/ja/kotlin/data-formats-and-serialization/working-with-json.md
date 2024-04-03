---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:43.379800-07:00
description: "Kotlin\u3067\u306EJSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:42.094563-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u3067\u306EJSON\uFF08JavaScript Object Notation\uFF09\u306E\u6271\
  \u3044\u65B9\u306F\u3001JSON\u30C7\u30FC\u30BF\u306E\u30D1\u30FC\u30B9\uFF08\u89E3\
  \u6790\uFF09\u3068\u751F\u6210\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001JSON\u306E\u8EFD\u91CF\u306A\u5F62\u5F0F\u3068\u4EBA\
  \u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u5F62\u5F0F\u306E\u305F\u3081\u3001\u7570\
  \u306A\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5C64\u9593\u3067\u30C7\
  \u30FC\u30BF\u3092\u7C21\u5358\u306B\u4EA4\u63DB\u3057\u305F\u308A\u3001Web\u30B5\
  \u30FC\u30D3\u30B9\u3068\u901A\u4FE1\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
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
