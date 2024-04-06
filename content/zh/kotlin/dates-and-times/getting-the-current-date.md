---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:23.948109-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Kotlin \u6CA1\u6709\u81EA\u5DF1\u7684\
  \u65E5\u671F\u548C\u65F6\u95F4 API\uFF0C\u4F46\u4F9D\u8D56\u4E8E Java \u6807\u51C6\
  \u5E93\u6765\u63D0\u4F9B\u6B64\u529F\u80FD\u3002\u4EE5\u4E0B\u662F\u83B7\u53D6\u5F53\
  \u524D\u65E5\u671F\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:46.893979-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Kotlin \u6CA1\u6709\u81EA\u5DF1\u7684\u65E5\
  \u671F\u548C\u65F6\u95F4 API\uFF0C\u4F46\u4F9D\u8D56\u4E8E Java \u6807\u51C6\u5E93\
  \u6765\u63D0\u4F9B\u6B64\u529F\u80FD\u3002\u4EE5\u4E0B\u662F\u83B7\u53D6\u5F53\u524D\
  \u65E5\u671F\u7684\u65B9\u6CD5\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 如何操作：


### 使用标准 Kotlin
Kotlin 没有自己的日期和时间 API，但依赖于 Java 标准库来提供此功能。以下是获取当前日期的方法：

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("今天的日期：$today")
}
```

**示例输出：**
```
今天的日期：2023-04-05
```

### 使用 java.util.Date
对于需要同时操作日期和时间的操作，你可能更喜欢 `java.util.Date`。

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("当前日期和时间：$currentDate")
}
```

**示例输出：**
```
当前日期和时间：周三 4月 05 15:20:45 GMT 2023
```

### 使用 Joda-Time 库
在 Java 8 引入新的日期和时间 API 之前，Joda-Time 是 Java 和 Kotlin 中日期时间操作的事实标准。尽管对许多项目来说不再必要，但出于遗留原因或个人偏好，一些项目可能仍会使用它。

将 Joda-Time 库添加到项目的 build.gradle 文件中：
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("今天的日期：$today")
}
```

**示例输出：**
```
今天的日期：2023-04-05
```

### 对于 Android 使用 ThreeTenABP
对于 Android 开发，建议在 Android API 级别 26 之前的版本中通过 ThreeTen Android Backport 项目回溯使用 Java 时间 API。

将依赖项添加到应用的 build.gradle 文件中：
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

在你的 Application 类中初始化它：
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

然后，你可以这样使用它：
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("今天的日期：$today")
}
```

**示例输出：**
```
今天的日期：2023-04-05
```
