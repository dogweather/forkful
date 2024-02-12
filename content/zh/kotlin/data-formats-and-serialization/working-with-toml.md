---
title:                "使用TOML"
date:                  2024-01-26T04:23:59.714153-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
TOML 代表 Tom's Obvious, Minimal Language（汤姆的明显、最小语言）。它用于配置文件，因为它易于人类阅读和编写，同时也易于机器解析。开发者选择 TOML 来避免 XML 的杂乱和 JSON 的棘手，进行配置管理。

## 如何操作：
要在 Kotlin 中处理 TOML，你可以使用像 `ktoml` 这样的库。首先，让我们在 `build.gradle.kts` 中添加依赖项：

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

现在，让我们来解析一些 TOML：

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("数据库主机: $host")
    println("数据库端口: $port")
}
```

假设 `config.toml` 看起来像这样：

```toml
[database]
host = "localhost"
port = 5432
```

样本输出会是：

```
数据库主机: localhost
数据库端口: 5432
```

## 深入探索
TOML，由 GitHub 联合创始人 Tom Preston-Werner 在 2013 年提出，旨在比 YAML 更直观，比 JSON 更类型安全。它已经成为热门选择，尤其是在 Rust 的 `Cargo` 和 Go 的模块系统中。有替代方案吗？YAML 有更多特性，JSON 在许多编程语言中直接转换为对象，当然还有老好人 XML。至于实现，ktoml 下的 Apache 2.0 许可，是一个纯 Kotlin 库，并不携带 Java 库，提供了编写 TOML 的 DSL，不仅仅是读取。

## 参见
- TOML GitHub：https://github.com/toml-lang/toml
- ktoml GitHub：https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON：https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
