---
title:    "Swift: 创建临时文件"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么要创建临时文件？

在大型的Swift项目中，有时候需要创建临时文件来存储一些临时数据。这些数据可能在程序运行结束后就不再需要了，或者我们不想让其余的代码使用到它们。创建临时文件可以帮助我们更清晰地管理项目，同时也可以提高程序的性能。

## 如何创建临时文件？

创建临时文件其实非常简单，我们可以使用Swift内置的FileManager类来完成。下面是一个简单的例子：

```Swift
if let tempDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask).first {
    let tempFileURL = tempDirectory.appendingPathComponent("temp_file.txt")
    FileManager.default.createFile(atPath: tempFileURL.path, contents: nil, attributes: nil)
}
```

上面的代码首先获取了应用程序的缓存目录路径，然后将临时文件的文件名加到路径中，并最后使用FileManager类的createFile方法来创建临时文件。

## 深入了解创建临时文件

当我们创建临时文件时，实际上还有一些参数可以选择。例如，我们可以使用FileManager类的createTemporaryFile方法来创建临时文件，这样系统会自动为文件生成一个唯一的文件名。此外，我们也可以为临时文件设置相应的属性，如创建日期、权限等。

临时文件的创建也可以通过其他第三方库来实现，例如CocoaLumberjack等。

# 参考链接

- [FileManager类官方文档](https://developer.apple.com/documentation/foundation/filemanager)
- [CocoaLumberjack官方仓库](https://github.com/CocoaLumberjack/CocoaLumberjack)