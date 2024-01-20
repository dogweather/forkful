---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？ (`What & Why?`)

读取文本文件，如其名，就是获取和操作文件中的文本。程序员之所以这么做，主因是它允许我们解析、修改、分享和储存数据。

## 怎么做： (`How to:`)

在 Swift（当前版本）中，我们可以简单地使用 `String(contentsOfFile:)` 方法来读取文本文件。以下示例是把文件“example.txt”的内容读取到一个字符串中。

```Swift
import Foundation

if let filePath = Bundle.main.path(forResource: "example", ofType: "txt") {
  do {
    let contents = try String(contentsOfFile: filePath, encoding: .utf8)
    print(contents)
  } catch {
    // 出错处理
    print("读取文件出错: \(error)")
  }
}
```

## 深入探索 (`Deep Dive`)

读取文本文件的实践在计算机编程的发展历程中可谓悠久。早期计算机系统使用的是 punch cards 和 paper tapes，上面记录着机器代码和数据。现代计算机系统则转向了方便理解和处理的像文本文件这样的格式。

除此之外，有一些备选方案可以满足读取和处理文件数据的需求：比如通过 `NSData` 或者 `InputStream` 读取数据流，或者使用像 `FileHandle` 这样的类来获取更精细的控制。

在 Swift 中，`String(contentsOfFile:)` 方法实际上是在内部封装了 `NSData`，然后转换成了 `String`。你可以指定读取的编码方式（例如 .utf8， .ascii 等等），如果不指定则 Swift 会试图推断出正确的编码方式。

## 另见 (`See Also`)

- [Apple 官方关于 String 的文档](https://developer.apple.com/documentation/swift/string)
- [stackoverflow: Swift 中文件的读写](https://stackoverflow.com/questions/24097826/read-and-write-a-string-from-text-file)

欢迎继续探索并深入学习 Swift 的世界。