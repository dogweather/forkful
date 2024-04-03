---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:08.701295-07:00
description: "YAML\uFF08YAML Ain't Markup\u2026"
lastmod: '2024-03-13T22:44:42.093498-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF08YAML Ain't Markup Language\u306E\u7565\uFF09\u306F\u3001\u8A2D\
  \u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30C7\u30FC\u30BF\u30B9\u30C8\u30EC\u30FC\u30B8\
  \u3084\u30D7\u30ED\u30BB\u30B9\u9593\u30E1\u30C3\u30BB\u30FC\u30B8\u30F3\u30B0\u306B\
  \u3088\u304F\u4F7F\u7528\u3055\u308C\u308B\u3001\u8AAD\u307F\u3084\u3059\u3044\u30C7\
  \u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u53EF\u8AAD\u6027\
  \u304C\u91CD\u8981\u306A\u5834\u5408\u306BJSON\u3084XML\u3088\u308A\u3082\u660E\u78BA\
  \u3055\u3068\u30B7\u30F3\u30D7\u30EB\u3055\u306E\u30E1\u30EA\u30C3\u30C8\u3092\u4EAB\
  \u53D7\u3057\u306A\u304C\u3089\u3001\u8A2D\u5B9A\u3068\u8A2D\u5B9A\u3092\u69CB\u9020\
  \u5316\u3055\u308C\u305F\u304C\u5206\u304B\u308A\u3084\u3059\u3044\u65B9\u6CD5\u3067\
  \u7BA1\u7406\u3059\u308B\u305F\u3081\u306B\u3001\u983B\u7E41\u306BYAML\u3092\u6271\
  \u3044\u307E\u3059\u3002."
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
