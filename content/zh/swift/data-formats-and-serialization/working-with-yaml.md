---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:05.344267-07:00
description: "YAML\uFF0C\u5168\u79F0\u662FYAML Ain't Markup Language\uFF08YAML\u4E0D\
  \u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u4E2A\u5BF9\u6240\u6709\u7F16\
  \u7A0B\u8BED\u8A00\u90FD\u53CB\u597D\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\
  \u5199\u3001\u8FDB\u7A0B\u95F4\u6D88\u606F\u4F20\u9012\u4EE5\u53CA\u6570\u636E\u5B58\
  \u50A8\uFF0C\u56E0\u4E3A\u5176\u53EF\u8BFB\u6027\u4E0E\u666E\u901A\u82F1\u8BED\u76F8\
  \u6BD4\u66F4\u63A5\u8FD1\uFF0C\u4E0E\u5176\u4ED6\u6570\u636E\u683C\u5F0F\u5982XML\u6216\
  JSON\u76F8\u6BD4\uFF0C\u5B83\u66F4\u7B80\u5355\u6613\u61C2\u548C\u7F16\u5199\u3002"
lastmod: '2024-03-13T22:44:48.181298-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5168\u79F0\u662FYAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\
  \u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u4E2A\u5BF9\u6240\u6709\u7F16\u7A0B\
  \u8BED\u8A00\u90FD\u53CB\u597D\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\u3002\
  \u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\u5199\
  \u3001\u8FDB\u7A0B\u95F4\u6D88\u606F\u4F20\u9012\u4EE5\u53CA\u6570\u636E\u5B58\u50A8\
  \uFF0C\u56E0\u4E3A\u5176\u53EF\u8BFB\u6027\u4E0E\u666E\u901A\u82F1\u8BED\u76F8\u6BD4\
  \u66F4\u63A5\u8FD1\uFF0C\u4E0E\u5176\u4ED6\u6570\u636E\u683C\u5F0F\u5982XML\u6216\
  JSON\u76F8\u6BD4\uFF0C\u5B83\u66F4\u7B80\u5355\u6613\u61C2\u548C\u7F16\u5199\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 什么和为什么？
YAML，全称是YAML Ain't Markup Language（YAML不是标记语言），是一个对所有编程语言都友好的数据序列化标准。程序员使用它进行配置文件编写、进程间消息传递以及数据存储，因为其可读性与普通英语相比更接近，与其他数据格式如XML或JSON相比，它更简单易懂和编写。

## 如何操作：
Swift 默认不包括对 YAML 解析和序列化的支持，因此需要使用第三方库。一个受欢迎的选择是 `Yams`，这是一个在 Swift 中操作 YAML 的库。

首先，你需要将 `Yams` 添加到你的项目中。如果你在使用 Swift 包管理器，可以在你的 `Package.swift` 文件中添加它作为依赖项：

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### 将 YAML 解析为 Swift
假设你有以下简单应用程序的 YAML 配置：

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

以下是如何使用 `Yams` 在 Swift 中解析这个 YAML 字符串：

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // 示例访问解析后的数据
        if let name = data["name"] as? String {
            print("App 名称：\(name)")
        }
    }
} catch {
    print("解析 YAML 错误：\(error)")
}
```

示例输出：

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
App 名称：MyApp
```

### 将 Swift 对象序列化为 YAML
使用 `Yams` 将 Swift 对象转回 YAML 字符串也很简单。假设你有需要被序列化的相同数据结构：

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("序列化为 YAML 错误：\(error)")
}
```

这将产生一个 YAML 格式的字符串：

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

这些示例展示了在 Swift 应用程序中使用 YAML 进行基本操作。记住，虽然 YAML 在人类可读性和易用性方面表现出色，但在选择你的数据序列化格式时，始终要考虑你的应用程序的特定需求，特别是在性能和复杂性方面。
