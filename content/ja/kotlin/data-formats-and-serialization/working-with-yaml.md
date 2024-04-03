---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:08.701295-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.093498-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u306B\u306FYAML\u306E\u89E3\u6790\u3084\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u306E\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30B5\
  \u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001`snakeyaml`\uFF08\
  \u4E00\u822C\u7684\u306AYAML\u89E3\u6790\u7528\uFF09\u3084`kotlinx.serialization`\uFF08\
  YAML\u5F62\u5F0F\u306E\u62E1\u5F35\u3092\u4F7F\u7528\uFF09\u306A\u3069\u306E\u4EBA\
  \u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u5229\u7528\u3057\u3066YAML\u30D5\u30A1\u30A4\u30EB\u3092\u6271\
  \u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## どのようにして：
KotlinにはYAMLの解析やシリアライゼーションのための組み込みサポートはありませんが、`snakeyaml`（一般的なYAML解析用）や`kotlinx.serialization`（YAML形式の拡張を使用）などの人気のあるサードパーティライブラリを利用してYAMLファイルを扱うことができます。

### `snakeyaml`を使用する
**依存関係：**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**YAMLを読む：**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// 使い方のサンプル
fun main() {
    readYaml("config.yaml")
}
```
**`config.yaml`のサンプル：**
```yaml
database:
  host: localhost
  port: 5432
```
**サンプル出力：**
```
{database={host=localhost, port=5432}}
```

### `kotlinx.serialization`をYAMLで使用する
まず、適切なYAMLサポートライブラリを含む`kotlinx-serialization`ライブラリがあることを確認してください（`kotlinx.serialization`は主にJSONおよびその他の形式を直接対象としていますが、使用可能であれば、YAMLサポートや代替ライブラリもチェックしてください）。

**依存関係：**
```kotlin
// JSON用（説明のため、YAMLサポートや代替ライブラリがあるかチェックしてください）
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**シリアライズ可能なデータクラスを定義する：**
```kotlin
import kotlinx.serialization.Serializable

@Serializable
data class Config(
    val database: Database
)

@Serializable
data class Database(
    val host: String,
    val port: Int
)
```

残念ながら、執筆時点で`kotlinx.serialization`における直接のYAMLサポートは限定的であるか進化している可能性があります。`snakeyaml`でYAMLをJSONに変換してから`kotlinx.serialization`でJSONをパースするような中間表現を使用する必要があるかもしれませんし、`kotlinx.serialization`と互換性のあるコミュニティ主導のYAMLシリアライゼーションプロジェクトを探す必要があります。

JSONの場合、コードは次のようになります：
```kotlin
import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString

fun main() {
    val jsonText = """
    {
        "database": {
            "host": "localhost",
            "port": 5432
        }
    }
    """.trimIndent()
    
    val config = Json.decodeFromString<Config>(jsonText)
    println(config)
}
```

Kotlinとそのエコシステムが進化し続けるにつれて、YAMLサポートとライブラリの最新情報について公式ドキュメントやコミュニティリソースをチェックし続けてください。
