---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:13.649974-07:00
description: "\u5728 Swift \u4E2D\u5904\u7406 JSON \u610F\u5473\u7740\u5904\u7406\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528 JSON \u5728\u670D\u52A1\u5668\u548C\u7F51\u7EDC\u5E94\u7528\u7A0B\
  \u5E8F\u4E4B\u95F4\u4F20\u8F93\u6570\u636E\uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u9605\
  \u8BFB\u548C\u89E3\u6790\uFF0C\u5BF9\u4EBA\u548C\u673A\u5668\u90FD\u5F88\u53CB\u597D\
  \u3002"
lastmod: '2024-03-13T22:44:48.182595-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Swift \u4E2D\u5904\u7406 JSON \u610F\u5473\u7740\u5904\u7406\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528 JSON \u5728\u670D\u52A1\u5668\u548C\u7F51\u7EDC\u5E94\u7528\u7A0B\
  \u5E8F\u4E4B\u95F4\u4F20\u8F93\u6570\u636E\uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u9605\
  \u8BFB\u548C\u89E3\u6790\uFF0C\u5BF9\u4EBA\u548C\u673A\u5668\u90FD\u5F88\u53CB\u597D\
  \u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Swift 中处理 JSON 意味着处理一种轻量级的数据交换格式。程序员使用 JSON 在服务器和网络应用程序之间传输数据，因为它易于阅读和解析，对人和机器都很友好。

## 如何操作：

Swift 使用 `Codable` 协议使得 JSON 解析变得简单直接。下面是如何将 JSON 解码为 Swift 对象的方法：

```Swift
import Foundation

// 定义一个遵循 Codable 的模型
struct User: Codable {
    var name: String
    var age: Int
}

// JSON 字符串
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// 将 JSON 字符串转换为 Data
if let jsonData = jsonString.data(using: .utf8) {
    // 将 JSON 数据解码为 User 对象
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("姓名：\(user.name)，年龄：\(user.age)")
    } catch {
        print("解码 JSON 出错：\(error)")
    }
}
```

示例输出：
```
姓名：John Doe，年龄：30
```

## 深入探究

自从 Douglas Crockford 在 2000 年代初期指定 JSON（JavaScript 对象表示法）以来，它已被广泛采用。它由于语法更简单、性能更好，已在许多用例中取代 XML。虽然 Swift 的 `Codable` 是处理 JSON 的首选，但当处理非 Codable 兼容类型时，还存在其他选择，如 `JSONSerialization`。在底层，`Codable` 抽象了较低级别的解析，并使得序列化/反序列化变得无缝。

## 另请参阅

- 在官方 Swift 博客中进一步探索有关 JSON 和 Swift 的信息：[Swift.org](https://swift.org/blog/)
- 查看 `Codable` 文档：[Swift Codable](https://developer.apple.com/documentation/swift/codable)
- 对于复杂的 JSON 结构，考虑使用诸如 SwiftyJSON 这样的第三方库，可在 [GitHub](https://github.com/SwiftyJSON/SwiftyJSON) 上找到。
