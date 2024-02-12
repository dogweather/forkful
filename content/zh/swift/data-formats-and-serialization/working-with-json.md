---
title:                "使用JSON进行编程"
date:                  2024-02-03T19:24:13.649974-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
