---
title:                "检查目录是否存在"
html_title:           "Swift: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么
一个常见的任务是要检查给定的文件夹是否存在。这是因为在Swift中，文件或文件夹操作往往需要确认它们是否存在，才能进行后续操作，以避免错误和不必要的操作。

## 怎么做
在Swift中，我们可以使用`FileManager`类来检查文件夹的存在性。首先，我们需要先创建一个对应的文件管理器对象，然后使用`fileExists`方法来检查文件夹是否存在。

```Swift
// 创建文件管理器对象
let fileManager = FileManager.default

// 检查文件夹是否存在
if fileManager.fileExists(atPath: "path/to/directory") {
    print("文件夹存在！")
} else {
    print("文件夹不存在！")
}
```

输出结果将会是`文件夹存在！`或者`文件夹不存在！`，取决于给定的文件夹是否存在。

## 深入探讨
在Swift中，我们也可以使用`path`属性来检查文件夹的存在性。此外，我们还可以使用`isExecutableFile`方法来检查文件夹是否可执行。

```Swift
// 使用path来检查文件夹存在性
let directoryPath = "path/to/directory"
if fileManager.fileExists(atPath: directoryPath.path) {
    print("文件夹存在！")
} else {
    print("文件夹不存在！")
}

// 使用isExecutableFile来检查文件夹是否可执行
if fileManager.isExecutableFile(atPath: directoryPath.path) {
    print("文件夹可执行！")
} else {
    print("文件夹不可执行！")
}
```

更多关于`FileManager`类的方法和属性，可以参考官方文档。

## 参考链接
- [FileManager - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Understanding the File System - Swift.org](https://swift.org/blog/understanding-file-systems/)
- [Swift FileManager and useful methods - Medium](https://medium.com/@maximbilan/filemanager-swift-400e2a5849e1)

## 参见