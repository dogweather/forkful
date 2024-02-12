---
title:                "使用YAML工作"
aliases:
- /zh/kotlin/working-with-yaml/
date:                  2024-02-03T19:25:58.023856-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？
YAML，即YAML Ain't Markup Language（YAML不是标记语言），是一种高可读性的数据序列化格式，常用于配置文件、数据存储和进程间消息传递。程序员经常使用YAML来以结构化而简单的方式管理配置和设置，当可读性重要时，YAML因其清晰和简单性而优于JSON或XML。

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
