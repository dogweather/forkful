---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:58.023856-07:00
description: "\u5982\u4F55\u505A\uFF1A Kotlin\u6CA1\u6709\u5185\u7F6E\u5BF9YAML\u89E3\
  \u6790\u548C\u5E8F\u5217\u5316\u7684\u652F\u6301\uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\
  \u7528\u6D41\u884C\u7684\u7B2C\u4E09\u65B9\u5E93\uFF0C\u6BD4\u5982`snakeyaml`\uFF08\
  \u7528\u4E8E\u4E00\u822C\u7684YAML\u89E3\u6790\uFF09\u548C`kotlinx.serialization`\uFF08\
  \u5E26\u6709YAML\u683C\u5F0F\u6269\u5C55\uFF09\u6765\u5904\u7406YAML\u6587\u4EF6\
  \u3002 **\u4F9D\u8D56\uFF1A**."
lastmod: '2024-03-13T22:44:47.742396-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u6CA1\u6709\u5185\u7F6E\u5BF9YAML\u89E3\u6790\u548C\u5E8F\u5217\u5316\
  \u7684\u652F\u6301\uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u6D41\u884C\u7684\u7B2C\
  \u4E09\u65B9\u5E93\uFF0C\u6BD4\u5982`snakeyaml`\uFF08\u7528\u4E8E\u4E00\u822C\u7684\
  YAML\u89E3\u6790\uFF09\u548C`kotlinx.serialization`\uFF08\u5E26\u6709YAML\u683C\u5F0F\
  \u6269\u5C55\uFF09\u6765\u5904\u7406YAML\u6587\u4EF6."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何做：
Kotlin没有内置对YAML解析和序列化的支持，但你可以使用流行的第三方库，比如`snakeyaml`（用于一般的YAML解析）和`kotlinx.serialization`（带有YAML格式扩展）来处理YAML文件。

### 使用`snakeyaml`
**依赖：**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**读取YAML：**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// 示例使用
fun main() {
    readYaml("config.yaml")
}
```
**示例`config.yaml`：**
```yaml
database:
  host: localhost
  port: 5432
```
**示例输出：**
```
{database={host=localhost, port=5432}}
```

### 使用`kotlinx.serialization`与YAML
首先，确保你有适合YAML支持库的`kotlinx-serialization`库（如果可用，因为`kotlinx.serialization`主要针对JSON和其他格式）。

**依赖：**
```kotlin
// 对于JSON（举例，检查YAML支持或替代库）
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**定义一个可序列化的数据类：**
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

不幸的是，在撰写本文时，`kotlinx.serialization`中对YAML的直接支持可能有限或正在发展中。你可能需要使用中间表示（例如，使用`snakeyaml`将YAML转换为JSON，然后用`kotlinx.serialization`解析JSON）或寻找与`kotlinx.serialization`兼容的社区驱动的YAML序列化项目。

对于JSON，代码看起来可能是这样的：
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

随着Kotlin及其生态系统的不断发展，继续关注官方文档和社区资源，以获取最新的YAML支持和库信息。
