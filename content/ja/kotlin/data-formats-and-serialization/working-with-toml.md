---
date: 2024-01-26 04:23:55.140798-07:00
description: "TOML\u306FTom's Obvious, Minimal Language\u306E\u7565\u3067\u3059\u3002\
  \u4EBA\u9593\u304C\u8AAD\u307F\u66F8\u304D\u3057\u3084\u3059\u304F\u3001\u540C\u6642\
  \u306B\u6A5F\u68B0\u304C\u89E3\u6790\u3057\u3084\u3059\u3044\u305F\u3081\u3001\u8A2D\
  \u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u958B\
  \u767A\u8005\u306F\u3001\u8A2D\u5B9A\u3092\u6271\u3046\u969B\u306BXML\u306E\u7169\
  \u96D1\u3055\u3084JSON\u306E\u7D30\u304B\u306A\u6280\u5DE7\u3092\u907F\u3051\u308B\
  \u305F\u3081\u306BTOML\u3092\u9078\u3073\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.096885-06:00'
model: gpt-4-0125-preview
summary: "TOML\u306FTom's Obvious, Minimal Language\u306E\u7565\u3067\u3059\u3002\u4EBA\
  \u9593\u304C\u8AAD\u307F\u66F8\u304D\u3057\u3084\u3059\u304F\u3001\u540C\u6642\u306B\
  \u6A5F\u68B0\u304C\u89E3\u6790\u3057\u3084\u3059\u3044\u305F\u3081\u3001\u8A2D\u5B9A\
  \u30D5\u30A1\u30A4\u30EB\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u958B\u767A\
  \u8005\u306F\u3001\u8A2D\u5B9A\u3092\u6271\u3046\u969B\u306BXML\u306E\u7169\u96D1\
  \u3055\u3084JSON\u306E\u7D30\u304B\u306A\u6280\u5DE7\u3092\u907F\u3051\u308B\u305F\
  \u3081\u306BTOML\u3092\u9078\u3073\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
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
