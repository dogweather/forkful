---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:08.701295-07:00
description: ''
lastmod: '2024-04-05T21:59:54.397227-06:00'
model: gpt-4-0125-preview
summary: ''
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
