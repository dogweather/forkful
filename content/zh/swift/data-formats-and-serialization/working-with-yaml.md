---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:05.344267-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Swift \u9ED8\u8BA4\u4E0D\u5305\u62EC\u5BF9\
  \ YAML \u89E3\u6790\u548C\u5E8F\u5217\u5316\u7684\u652F\u6301\uFF0C\u56E0\u6B64\u9700\
  \u8981\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u3002\u4E00\u4E2A\u53D7\u6B22\u8FCE\u7684\
  \u9009\u62E9\u662F `Yams`\uFF0C\u8FD9\u662F\u4E00\u4E2A\u5728 Swift \u4E2D\u64CD\
  \u4F5C YAML \u7684\u5E93\u3002 \u9996\u5148\uFF0C\u4F60\u9700\u8981\u5C06 `Yams`\
  \ \u6DFB\u52A0\u5230\u4F60\u7684\u9879\u76EE\u4E2D\u3002\u5982\u679C\u4F60\u5728\
  \u4F7F\u7528 Swift \u5305\u7BA1\u7406\u5668\uFF0C\u53EF\u4EE5\u5728\u4F60\u7684\
  \ `Package.swift`\u2026"
lastmod: '2024-04-05T22:38:47.329458-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Swift \u9ED8\u8BA4\u4E0D\u5305\u62EC\u5BF9\
  \ YAML \u89E3\u6790\u548C\u5E8F\u5217\u5316\u7684\u652F\u6301\uFF0C\u56E0\u6B64\u9700\
  \u8981\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u3002\u4E00\u4E2A\u53D7\u6B22\u8FCE\u7684\
  \u9009\u62E9\u662F `Yams`\uFF0C\u8FD9\u662F\u4E00\u4E2A\u5728 Swift \u4E2D\u64CD\
  \u4F5C YAML \u7684\u5E93\u3002 \u9996\u5148\uFF0C\u4F60\u9700\u8981\u5C06 `Yams`\
  \ \u6DFB\u52A0\u5230\u4F60\u7684\u9879\u76EE\u4E2D\u3002\u5982\u679C\u4F60\u5728\
  \u4F7F\u7528 Swift \u5305\u7BA1\u7406\u5668\uFF0C\u53EF\u4EE5\u5728\u4F60\u7684\
  \ `Package.swift` \u6587\u4EF6\u4E2D\u6DFB\u52A0\u5B83\u4F5C\u4E3A\u4F9D\u8D56\u9879\
  \uFF1A."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
