---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？

创建临时文件是在计算机的临时存储位置新建一个文件的行为。程序员创建临时文件有很多原因，比如缓存数据、进行大型计算，或者存储应用程序运行期间产生的短暂数据。

## 如何实现：

在 Swift 中创建临时文件的代码如下：

```Swift
import Foundation

let tempDirectoryURL = NSURL.fileURL(withPath: NSTemporaryDirectory(), isDirectory: true)
let targetURL = tempDirectoryURL.appendingPathComponent(UUID().uuidString)

do {
    try "Hello, Swift!".write(to: targetURL, atomically: true, encoding: .utf8)
    print("Temp file: \(targetURL)")
} catch {
    print("An error occured: \(error)")
}
```

执行以上代码，你会在控制台看到如下输出：

```Swift
Temp file: file:///var/folders/xx/xx/xx/C/com.apple.dt.Xcode.pg/resources/12345678-90ab-cdef-1234-567890abcdef
```

## 深入探讨：

在计算机编程的早期，临时文件是程序员为存储短暂数据所创建的文件。Swift 提供了 `NSTemporaryDirectory()` 方法创建临时文件，这是一种简单的方式。但是，也有其他替代方式。例如，我们可以使用 `URLSession` 在下载时直接写入临时文件，或者使用 `FileManager` 手动创建临时目录。

上面的代码中，`UUID().uuidString` 用于生成一个唯一的文件名，以确保不会覆盖已有的临时文件。`NSTemporaryDirectory()` 可用于获取临时文件的路径。我们通过 `write(to:atomically:encoding:)` 方法将字符串写入这个文件中，这个方法会覆盖目标 URL，保证数据的原子性一致。

## 参考资料：

以下是一些相关的学习资源链接：

1. Apple Developer Documentation: [Creating Temporary Files in the File System](https://developer.apple.com/documentation/foundation/nsfilemanager/1412643-url)
2. Swift by Sundell: [Working with files in Swift](https://www.swiftbysundell.com/posts/working-with-files-in-swift)
3. "Use Your Loaf": [Working with Directories in Swift](https://useyourloaf.com/blog/working-with-directories-in-swift/)