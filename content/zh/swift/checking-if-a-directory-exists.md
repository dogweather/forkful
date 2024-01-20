---
title:                "检查目录是否存在"
html_title:           "PHP: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么是目录存在检查以及为何这么做？
在 Swift 编程中，目录存在检查指的是确认某一特定文件夹在设备上实际存在的操作。编程者这么做主要是为了预防出错情况发生，如尝试访问不存在的文件或目录。

## 如何操作:
我们可以使用 Swift 的 `FileManager` 对象直接检查指定路径的文件夹是否存在。以下是代码示例和输出样例：
```Swift
import Foundation

let fileManager = FileManager.default
let folderPath = "/path/to/directory"

if fileManager.fileExists(atPath: folderPath) {
    print("Directory exists! 😀")
} else {
    print("Directory does not exist! 😳")
}
```
当你提供的文件夹路径根据实际情况存在或不存在时，程序将打印出相应的信息。

## 深入理解:
 historically，Swift 的早期版本没有内置的目录存在性检查方法。开发者需要自己写低级别的代码来检查一个文件夹是否存在。然而，现在通过 `FileManager` 类，这一复杂的操作变得更加简洁、高效。

真的需要检查目录存在么? 其实，出于一些情况，你可能不需要这一步。例如，当你尝试创建一个新文件夹时，如果不存在，那 Swift 将自动为你创建。但是，其他情况下，例如读取或删除目录，预先检查则会降低应用出错的风险。

实际上，`fileExists(atPath:)`方法在内部通过调用系统的低级别函数来判断文件或目录是否存在。为了让这一进程更高效，Swift 实现了缓存策略，即当同一路径被多次检查时，操作速度将加快。

## 更多相关资源:
1. Apple Developer Documentation: [fileExists(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1410277-fileexists)
2. Swift by Sundell: [Working with files in Swift](https://www.swiftbysundell.com/basics/working-with-files-in-swift/)
3. Stack Overflow thread: [Checking if a folder exists in Swift](https://stackoverflow.com/questions/24097826/read-and-write-a-string-from-text-file)
4. Swift Tutorial: [How to use FileManager in Swift](https://learnappmaking.com/filemanager-swift-how-to/)