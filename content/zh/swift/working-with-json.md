---
title:                "使用 json 进行编程"
html_title:           "Swift: 使用 json 进行编程"
simple_title:         "使用 json 进行编程"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-json.md"
---

{{< edit_this_page >}}

## 为什么：

JSON是一种非常流行的数据交换格式，它经常用于从服务器获取数据。如果你想要开发iOS应用程序和网络服务，那么了解JSON将会非常有帮助。

## 如何操作：

在Swift中，你可以使用内置的JSON解析器来处理JSON数据。假设你从服务器收到以下JSON数据：

```Swift
let json = """
{
    "name": "John Smith",
    "age": 25,
    "occupation": "Software Engineer"
}
"""

```

你可以使用如下代码来将JSON转换成一个Dictionary：

```Swift
if let data = json.data(using: .utf8) {
    do {
        let dictionary = try JSONSerialization.jsonObject(with: data, options: .allowFragments) as? [String: Any]
        print(dictionary) // 输出：["name": "John Smith", "age": 25, "occupation": "Software Engineer"]
    } catch {
        print(error)
    }
}
```

如果你想要将一个自定义对象转换为JSON，你可以遵循Codable协议，并使用JSONEncoder将对象编码为JSON，或使用JSONDecoder将JSON解码为对象。示例如下：

```Swift
class Person: Codable {
    var name: String
    var age: Int
    var occupation: String
    
    init(name: String, age: Int, occupation: String) {
        self.name = name
        self.age = age
        self.occupation = occupation
    }
}

let person = Person(name: "Jane Doe", age: 30, occupation: "Teacher")

if let jsonData = try? JSONEncoder().encode(person) {
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print(jsonString) // 输出：{"name":"Jane Doe","age":30,"occupation":"Teacher"}
    }
}

let jsonStr = """
{
    "name": "Alex Smith",
    "age": 35,
    "occupation": "Doctor"
}
"""

if let jsonData = jsonStr.data(using: .utf8) {
    if let person = try? JSONDecoder().decode(Person.self, from: jsonData) {
        print(person.name) // 输出：Alex Smith
    }
}
```

## 深入探讨：

JSON可以表示多种数据类型，包括字符串、数字、布尔值、数组和嵌套的对象。如果你想要更深入地了解JSON的结构和如何处理嵌套数据，可以阅读易于理解的官方文档[Working with JSON in Swift](https://developer.apple.com/swift/blog/?id=37)。

## 参考链接：

- [JSON.org - Official JSON website](https://www.json.org)
- [JSON – Wikipedia](https://en.wikipedia.org/wiki/JSON)
- [Working with JSON in Swift - Apple Developer Documentation](https://developer.apple.com/swift/blog/?id=37)

## 更多阅读：

- [Codable in Swift: Beyond the basics](https://medium.com/@mzafrapp/codable-in-swift-beyond-the-basics-3f105df92b09)
- [Parsing JSON in Swift 4 - Medium](https://medium.com/@alfianlosari/parsing-json-in-swift-4-2-1-using-jsondecoder-2ea38964cde9) 
- [JSON Parsing using Swift - raywenderlich](https://www.raywenderlich.com/112189/json-parsing-in-swift-tutorial)