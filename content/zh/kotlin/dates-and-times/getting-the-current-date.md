---
aliases:
- /zh/kotlin/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:23.948109-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\
  \u4E00\u9879\u57FA\u672C\u4EFB\u52A1\uFF0C\u5B83\u4F7F\u5F00\u53D1\u4EBA\u5458\u80FD\
  \u591F\u8BBF\u95EE\u3001\u663E\u793A\u6216\u64CD\u4F5C\u5E94\u7528\u7A0B\u5E8F\u4E2D\
  \u7684\u5F53\u524D\u65E5\u671F\u3002\u4ECE\u8BB0\u5F55\u548C\u65F6\u95F4\u6233\u4E8B\
  \u4EF6\u5230\u57FA\u4E8E\u65E5\u671F\u7684\u8BA1\u7B97\uFF0C\u8FD9\u4E2A\u80FD\u529B\
  \u5BF9\u4E8E\u4EFB\u4F55\u4E8B\u60C5\u90FD\u662F\u81F3\u5173\u91CD\u8981\u7684\u3002"
lastmod: 2024-02-18 23:08:59.104415
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\
  \u4E00\u9879\u57FA\u672C\u4EFB\u52A1\uFF0C\u5B83\u4F7F\u5F00\u53D1\u4EBA\u5458\u80FD\
  \u591F\u8BBF\u95EE\u3001\u663E\u793A\u6216\u64CD\u4F5C\u5E94\u7528\u7A0B\u5E8F\u4E2D\
  \u7684\u5F53\u524D\u65E5\u671F\u3002\u4ECE\u8BB0\u5F55\u548C\u65F6\u95F4\u6233\u4E8B\
  \u4EF6\u5230\u57FA\u4E8E\u65E5\u671F\u7684\u8BA1\u7B97\uFF0C\u8FD9\u4E2A\u80FD\u529B\
  \u5BF9\u4E8E\u4EFB\u4F55\u4E8B\u60C5\u90FD\u662F\u81F3\u5173\u91CD\u8981\u7684\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在编程中，获取当前日期是一项基本任务，它使开发人员能够访问、显示或操作应用程序中的当前日期。从记录和时间戳事件到基于日期的计算，这个能力对于任何事情都是至关重要的。

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
