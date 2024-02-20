---
date: 2024-01-26 04:23:59.714153-07:00
description: "TOML \u4EE3\u8868 Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u660E\u663E\u3001\u6700\u5C0F\u8BED\u8A00\uFF09\u3002\u5B83\u7528\u4E8E\u914D\u7F6E\
  \u6587\u4EF6\uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u4EBA\u7C7B\u9605\u8BFB\u548C\u7F16\
  \u5199\uFF0C\u540C\u65F6\u4E5F\u6613\u4E8E\u673A\u5668\u89E3\u6790\u3002\u5F00\u53D1\
  \u8005\u9009\u62E9 TOML \u6765\u907F\u514D XML \u7684\u6742\u4E71\u548C JSON \u7684\
  \u68D8\u624B\uFF0C\u8FDB\u884C\u914D\u7F6E\u7BA1\u7406\u3002"
lastmod: 2024-02-19 22:05:06.779788
model: gpt-4-0125-preview
summary: "TOML \u4EE3\u8868 Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u660E\u663E\u3001\u6700\u5C0F\u8BED\u8A00\uFF09\u3002\u5B83\u7528\u4E8E\u914D\u7F6E\
  \u6587\u4EF6\uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u4EBA\u7C7B\u9605\u8BFB\u548C\u7F16\
  \u5199\uFF0C\u540C\u65F6\u4E5F\u6613\u4E8E\u673A\u5668\u89E3\u6790\u3002\u5F00\u53D1\
  \u8005\u9009\u62E9 TOML \u6765\u907F\u514D XML \u7684\u6742\u4E71\u548C JSON \u7684\
  \u68D8\u624B\uFF0C\u8FDB\u884C\u914D\u7F6E\u7BA1\u7406\u3002"
title: "\u4F7F\u7528TOML"
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
