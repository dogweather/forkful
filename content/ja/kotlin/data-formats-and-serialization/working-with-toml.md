---
date: 2024-01-26 04:23:55.140798-07:00
description: "\u65B9\u6CD5\uFF1A Kotlin\u3067TOML\u3092\u6271\u3046\u306B\u306F\u3001\
  `ktoml`\u306E\u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u307E\u305A\u3001`build.gradle.kts`\u306B\
  \u4F9D\u5B58\u95A2\u4FC2\u3092\u8FFD\u52A0\u3057\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-03-13T22:44:42.096885-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u3067TOML\u3092\u6271\u3046\u306B\u306F\u3001`ktoml`\u306E\u3088\u3046\
  \u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u307E\u305A\u3001`build.gradle.kts`\u306B\u4F9D\u5B58\
  \u95A2\u4FC2\u3092\u8FFD\u52A0\u3057\u307E\u3057\u3087\u3046\uFF1A."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## 方法：
KotlinでTOMLを扱うには、`ktoml`のようなライブラリを使用することができます。まず、`build.gradle.kts`に依存関係を追加しましょう：

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

次に、いくつかのTOMLを解析しましょう：

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("データベースのホスト: $host")
    println("データベースのポート: $port")
}
```

`config.toml`がこんな感じだとすると：

```toml
[database]
host = "localhost"
port = 5432
```

サンプル出力はこんな感じになります：

```
データベースのホスト: localhost
データベースのポート: 5432
```

## 深掘り
TOMLは、GitHubの共同創設者であるTom Preston-Wernerによって2013年に考案され、YAMLよりも直截的でJSONよりも型安全を目指していました。Rustの`Cargo`やGoのモジュールシステムといった場で特に支持されています。代替案？YAMLにはより多くの機能があり、JSONは多くのプログラミング言語でオブジェクトに直接変換され、そして常に良い古いXMLがあります。実装において、ktomlはApache 2.0ライセンスのもとで公開されており、純粋なKotlinライブラリであり、Javaライブラリを引きずらずにTOMLを読むだけでなく書くためのDSLも提供します。

## 参照
- TOMLのGitHub：https://github.com/toml-lang/toml
- ktomlのGitHub：https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON：https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
