---
title:                "Swift: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

##为什么

写文本文件是 Swift 编程中普遍的一项任务。它可以让我们以文本格式保存数据，并在需要时轻松地读取这些数据。例如，在开发一个记事本应用时，我们可能需要将用户的笔记保存在文本文件中，这样用户就可以在下次打开应用时继续编辑他们的笔记。同时，文本文件还可以供其他程序访问，从而实现数据共享。

##如何

写文本文件可以通过 Swift 的 FileManager 类来完成。首先，我们需要使用 FileManager.default 创建一个 FileManager 的实例。然后，使用该实例的 createFile(atPath:contents:attributes:) 方法来创建文本文件，并指定文件路径、要写入的内容和文件属性。代码示例如下：

```Swift
import Foundation

// 创建 FileManager 实例
let fileManager = FileManager.default

// 指定文件路径
let filePath = "textFile.txt"

// 定义要写入的内容
let fileContent = "这是一段文本文件的内容！"

// 定义文件属性，例如创建日期、修改日期等（可选）
let fileAttributes = [FileAttributeKey.creationDate: Date(), FileAttributeKey.modificationDate: Date()]

// 使用 FileManager 实例创建文本文件，如果创建成功则打印成功信息
if fileManager.createFile(atPath: filePath, contents: fileContent.data(using: .utf8), attributes: fileAttributes) {
    print("文本文件创建成功！")
} else {
    print("文本文件创建失败！")
}
```

运行以上代码，我们就可以在当前路径下找到一个名为 textFile.txt 的文本文件，并且该文件的内容为 "这是一段文本文件的内容！"。如果需要读取文本文件的内容，可以使用 FileManager 的 contents(atPath:) 方法来实现。

##深入了解

除了使用 FileManager 类，我们还可以使用 Foundation 框架中的 NSString 类来写入文本文件。该类提供了 write(toFile:atomically:encoding:) 方法，通过指定文件路径、是否要原子写入（如果设置为 true，则会先将新文件写入临时文件，然后用临时文件替换旧文件。如果设置为 false，则直接写入旧文件。）以及编码方式（例如 utf8、utf16 等）来写入文本文件。代码示例如下：

```Swift
import Foundation

// 定义文件路径
let filePath = "textFile.txt"

// 定义要写入的内容
let fileContent = "这是一段文本文件的内容！"

// 使用 NSString 类写入文本文件，如果写入成功则打印成功信息
if (try? fileContent.write(toFile: filePath, atomically: true, encoding: .utf8)) != nil {
    print("文本文件写入成功！")
} else {
    print("文本文件写入失败！")
}
```

需要注意的是，无论是使用 FileManager 还是 NSString 类来写入文本文件，都需要指定文件路径，否则系统会将文件保存在临时文件夹中，且文件名是随机的。

##参考链接

- [FileManager - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [NSString - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsstring)
- [Creating and Writing to a File - Swift by Sundell](https://www.swiftbysundell.com/basics/creating-writing-to-files/)