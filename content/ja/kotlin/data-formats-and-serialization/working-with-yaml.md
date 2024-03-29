---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:08.701295-07:00
description: "YAML\uFF08YAML Ain't Markup\u2026"
lastmod: '2024-03-13T22:44:42.093498-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF08YAML Ain't Markup\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
YAML（YAML Ain't Markup Languageの略）は、設定ファイル、データストレージやプロセス間メッセージングによく使用される、読みやすいデータシリアライゼーション形式です。プログラマーは、可読性が重要な場合にJSONやXMLよりも明確さとシンプルさのメリットを享受しながら、設定と設定を構造化されたが分かりやすい方法で管理するために、頻繁にYAMLを扱います。

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
