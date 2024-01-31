---
title:                "处理JSON数据"
date:                  2024-01-19
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"

category:             "Swift"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在Swift中，使用JSON（JavaScript Object Notation）处理数据是常规做法，因为它轻量且易于交换。程序员利用它与服务器交互数据，实现应用与互联网的对接。

## How to: (怎么做？)
```Swift
import Foundation

// 定义一个匹配JSON结构的结构体
struct User: Codable {
    var name: String
    var age: Int
}

// 一个JSON字符串例子
let jsonString = """
{
    "name": "张三",
    "age": 28
}
"""

// 将JSON字符串转换为Swift对象
if let jsonData = jsonString.data(using: .utf8) {
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("姓名: \(user.name), 年龄: \(user.age)")
    } catch {
        print("解析错误: \(error)")
    }
}
```
执行以上代码，输出应该是：
```
姓名: 张三, 年龄: 28
```

## Deep Dive (深入探究)
JSON从2001年开始被使用，最初由Douglas Crockford提出，现已成为互联网数据交换的事实标准。在Swift中，我们经常使用`Codable`协议（`Decodable`和`Encodable`的组合）来简化序列化和反序列化过程。除了Swift的原生JSON处理，还有第三方库如`SwiftyJSON`和`ObjectMapper`可以考虑使用，但对于绝大多数场景，Swift标准库中的工具已经足够强大。

## See Also (另请参阅)
- [Swift的官方文档关于JSON](https://developer.apple.com/documentation/foundation/jsonserialization)
- [可编码和可解码类型官方指南](https://developer.apple.com/documentation/swift/codable)
- [SwiftyJSON GitHub](https://github.com/SwiftyJSON/SwiftyJSON)
- [ObjectMapper GitHub](https://github.com/tristanhimmelman/ObjectMapper)
