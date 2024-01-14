---
title:    "Swift: 检查目录是否存在"
keywords: ["Swift"]
---

{{< edit_this_page >}}

##为什么：只有 1-2 句话解释为什么有必要检查目录是否存在？

当我们写程序时，经常需要访问文件或目录。有时候我们需要确认一个目录是否存在，这样我们才能够在程序中继续操作它。因此，检查目录是否存在是非常重要的，可以避免程序运行出现错误。

##如何进行检查

```Swift
// 首先，我们需要引入Foundation框架
import Foundation

// 创建一个FileManager实例
let fileManager = FileManager.default

// 指定我们想要检查的目录路径
let directoryPath = "Documents/Notes"

// 使用fileExists(atPath:)方法来检查目录是否存在，返回一个布尔值
let fileExists = fileManager.fileExists(atPath: directoryPath)

if fileExists {
    print("目录存在")
} else {
    print("目录不存在")
}
```

输出结果：

```目录存在```

##深入了解

除了上述的基本示例，我们还可以使用更多的方法来检查目录是否存在。例如，我们可以使用fileExists(atPath:isDirectory:)方法来检查指定路径是否为一个目录，还可以使用fileExists(atPath:isSymbolicLink:)方法来检查指定路径是否为一个符号链接。同时，我们还可以使用fileExists(atPath:traverseLink:)方法来检查指定路径是否为一个符号链接或者一个实际文件。

另外，我们还可以使用fileExists(atPath:)方法的另一个版本，它接受一个URL类型的参数。这样可以更加灵活地操作路径。此外，我们还可以使用fileManager的attributesOfItem(atPath:)方法来获取指定路径相关的属性信息，例如文件大小、创建日期等等。

总的来说，检查目录是否存在并不仅仅局限于使用fileExists(atPath:)方法，我们可以根据具体的情况选择不同的方法来实现。

##参考链接

- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Path Checking](https://developer.apple.com/documentation/foundation/filemanager/path_checking)
- [How to check that a file or directory exists using FileManager](https://www.hackingwithswift.com/example-code/strings/how-to-check-that-a-file-or-directory-exists-using-filemanager)
- [Swift文件、文件管理详解](https://www.infoq.cn/article/swift-file-file-management-in-detail) 

##参阅：

- [File System Programming Guide](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html#//apple_ref/doc/uid/TP40010672-CH2-SW1)
- [文件管理文档指导](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html#//apple_ref/doc/uid/TP40010672-CH2-SW1)