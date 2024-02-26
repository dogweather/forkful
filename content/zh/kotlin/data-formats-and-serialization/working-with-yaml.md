---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:58.023856-07:00
description: "YAML\uFF0C\u5373YAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\u6807\
  \u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u9AD8\u53EF\u8BFB\u6027\u7684\u6570\
  \u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u5E38\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\
  \u3001\u6570\u636E\u5B58\u50A8\u548C\u8FDB\u7A0B\u95F4\u6D88\u606F\u4F20\u9012\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u4F7F\u7528YAML\u6765\u4EE5\u7ED3\u6784\u5316\u800C\
  \u7B80\u5355\u7684\u65B9\u5F0F\u7BA1\u7406\u914D\u7F6E\u548C\u8BBE\u7F6E\uFF0C\u5F53\
  \u53EF\u8BFB\u6027\u91CD\u8981\u65F6\uFF0CYAML\u56E0\u5176\u6E05\u6670\u548C\u7B80\
  \u5355\u6027\u800C\u4F18\u4E8EJSON\u6216XML\u3002"
lastmod: '2024-02-25T18:49:45.310023-07:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5373YAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\u6807\u8BB0\
  \u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u9AD8\u53EF\u8BFB\u6027\u7684\u6570\u636E\
  \u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u5E38\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u3001\
  \u6570\u636E\u5B58\u50A8\u548C\u8FDB\u7A0B\u95F4\u6D88\u606F\u4F20\u9012\u3002\u7A0B\
  \u5E8F\u5458\u7ECF\u5E38\u4F7F\u7528YAML\u6765\u4EE5\u7ED3\u6784\u5316\u800C\u7B80\
  \u5355\u7684\u65B9\u5F0F\u7BA1\u7406\u914D\u7F6E\u548C\u8BBE\u7F6E\uFF0C\u5F53\u53EF\
  \u8BFB\u6027\u91CD\u8981\u65F6\uFF0CYAML\u56E0\u5176\u6E05\u6670\u548C\u7B80\u5355\
  \u6027\u800C\u4F18\u4E8EJSON\u6216XML\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
