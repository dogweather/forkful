---
title:                "使用YAML工作"
aliases:
- zh/swift/working-with-yaml.md
date:                  2024-02-03T19:27:05.344267-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
