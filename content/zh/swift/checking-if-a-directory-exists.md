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

# 什么是检查目录是否存在?

检查目录是否存在是指在Swift编程中，通过一些方法和函数来判断一个特定的目录是否存在。程序员经常会这样做是为了确保程序能够正确地运行。

# 如何进行检查?

检查目录是否存在可以通过使用Swift的内置函数`fileExists(atPath path: String) -> Bool`来实现。这个函数接受一个字符串参数表示目录的路径，并返回布尔值来表示该目录是否存在。

```Swift
let fileManager = FileManager.default
let directoryPath = "path/to/directory"
if fileManager.fileExists(atPath: directoryPath) {
    print("目录存在！")
} else {
    print("目录不存在。")
}
```

接下来，让我们来看看一个更复杂的例子，我们可以在一个循环中检查目录是否存在，并根据结果做出不同的操作：

```Swift
let directories = ["/path/to/directory1", "/path/to/directory2", "/path/to/directory3"]
let fileManager = FileManager.default
for directory in directories {
    if fileManager.fileExists(atPath: directory) {
        print("\(directory)存在！")
        // 在这里添加你的操作代码
    } else {
        print("\(directory)不存在。")
        // 在这里添加你的操作代码
    }
}
```

在上面的代码中，我们首先建立了一个包含多个目录路径的数组，然后使用`fileExists(atPath:)`函数来检查每个目录是否存在。如果存在，我们可以在`if`语句块中执行相应的操作，如果不存在，我们可以在`else`语句块中执行其他操作。

# 深入了解

检查目录是否存在在编程中是一个常见的操作。在以前的编程语言中，需要使用更复杂的代码来检查目录是否存在，比如C语言中的`opendir()`函数。但是在Swift中，通过使用`FileManager`的`fileExists(atPath:)`函数，我们可以很容易地检查目录是否存在。

除了使用`fileExists(atPath:)`函数，我们也可以使用`FileManager`的其他方法来检查文件或目录的存在性，比如`fileExists(atPath:)`和`isReadableFile(atPath:)`等。

# 参考链接

- [Swift编程语言官方文档](https://docs.swift.org/swift-book)
- [Swift FileManager文档](https://developer.apple.com/documentation/foundation/filemanager)
- [如何在Swift中检查文件或目录是否存在](https://www.hackingwithswift.com/example-code/system/how-to-check-whether-a-file-or-directory-exists-using-filemanager)