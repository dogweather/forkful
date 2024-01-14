---
title:                "Swift: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么要创建临时文件

临时文件在编写Swift程序时经常会用到。它们是暂时存储数据的有效方法，可以帮助节省磁盘空间，同时也可以帮助我们在处理数据时保持程序的整洁。让我们来看看如何在Swift中创建临时文件。

# 如何创建临时文件

在Swift中，我们可以使用FileManager类来创建临时文件。首先，我们需要创建一个文件的路径，并将其存储在变量中。接下来，我们可以使用FileManager的createFile方法来创建一个临时文件，如下所示：

```Swift
// 创建临时文件的路径
let tempPath = NSTemporaryDirectory().appending("tempfile.txt")

// 使用FileManager创建临时文件
FileManager.default.createFile(atPath: tempPath, contents: nil, attributes: nil)
```

接下来，我们可以使用write方法向临时文件中写入数据，如下所示：

```Swift
// 定义要写入临时文件的数据
let data = "这是一个临时文件".data(using: .utf8)

// 使用FileManager的write方法将数据写入临时文件
FileManager.default.createFile(atPath: tempPath, contents: data, attributes: nil)
```

我们也可以使用FileManager的removeItem方法来删除临时文件，如下所示：

```Swift
// 使用FileManager的removeItem方法来删除临时文件
try? FileManager.default.removeItem(atPath: tempPath)
```

完成以上步骤后，我们就成功地创建并操作了一个临时文件。

# 深入了解临时文件

临时文件是指在程序执行过程中创建的临时文件，它们的存在时间通常只在程序执行期间。创建临时文件的路径通常是操作系统提供的临时文件夹，我们可以使用NSTemporaryDirectory方法来获取临时文件夹的路径。

临时文件也可以是被写入内容的本地文件，它们可以被用于缓存数据、临时存储用户设置等。但是请注意，由于临时文件的生命周期短暂，当程序重新启动时，它们将被删除。

# 查看也使用

- [Swift官方文档-FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [了解NSTemporaryDirectory方法](https://stackoverflow.com/questions/24097826/what-is-nstemporarydirectory-give-examples-using-it)
- [如何使用临时文件来缓存数据](https://www.appcoda.com.hk/temporary-files-data-caching/)