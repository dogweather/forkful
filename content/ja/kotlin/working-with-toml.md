---
title:                "TOMLを扱う方法"
date:                  2024-01-26T04:23:55.140798-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLはTom's Obvious, Minimal Languageの略です。人間が読み書きしやすく、同時に機械が解析しやすいため、設定ファイルに使用されます。開発者は、設定を扱う際にXMLの煩雑さやJSONの細かな技巧を避けるためにTOMLを選びます。

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
