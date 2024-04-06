---
date: 2024-01-26 04:23:59.714153-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 Kotlin \u4E2D\u5904\u7406\
  \ TOML\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u50CF `ktoml` \u8FD9\u6837\u7684\u5E93\
  \u3002\u9996\u5148\uFF0C\u8BA9\u6211\u4EEC\u5728 `build.gradle.kts` \u4E2D\u6DFB\
  \u52A0\u4F9D\u8D56\u9879\uFF1A."
lastmod: '2024-04-05T22:38:46.909156-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 Kotlin \u4E2D\u5904\u7406 TOML\uFF0C\
  \u4F60\u53EF\u4EE5\u4F7F\u7528\u50CF `ktoml` \u8FD9\u6837\u7684\u5E93\u3002\u9996\
  \u5148\uFF0C\u8BA9\u6211\u4EEC\u5728 `build.gradle.kts` \u4E2D\u6DFB\u52A0\u4F9D\
  \u8D56\u9879\uFF1A."
title: "\u4F7F\u7528TOML"
weight: 39
---

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
