---
title:                "创建临时文件"
html_title:           "Swift: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

在编写Swift程序时，有时我们需要在运行过程中创建临时文件来储存数据、临时记录日志等。创建临时文件可以帮助我们更有效地管理和处理数据，提高程序的运行效率。

## 如何操作

在Swift中，我们可以使用 `FileManager` 类来创建临时文件。下面是一个简单的例子：

```Swift
let fileManager = FileManager.default
let tempDirectoryURL = NSURL.fileURL(withPath: NSTemporaryDirectory())
let tempFileURL = tempDirectoryURL.appendingPathComponent("tempFile.txt")

do {
    // 在临时目录中创建文件
    try "Hello World".write(to: tempFileURL, atomically: true, encoding: String.Encoding.utf8)

    // 读取文件内容
    let data = try Data(contentsOf: tempFileURL)
    let fileContent = String(data: data, encoding: .utf8)

    // 打印输出
    print("临时文件路径：\(tempFileURL)")
    print("临时文件内容：\(fileContent)")
} catch {
    print("创建临时文件失败：\(error)")
}
```

运行以上代码，你会在控制台看到下面的输出：

```Swift
临时文件路径：file:///Users/username/Library/Developer/CoreSimulator/Devices/DEVICE_ID/data/Containers/Data/Application/APP_ID/tmp/tempFile.txt
临时文件内容：Hello World
```

这里使用了 `FileManager` 类的 `default` 属性来获取一个默认的文件管理对象。然后，我们通过 `NSURL` 的 `fileURL(withPath:)` 方法获取了一个指向临时目录的URL。接下来，使用 `appendingPathComponent(_:)` 方法在临时目录中创建了一个名为 "tempFile.txt" 的文件，并将 "Hello World" 写入其中。最后，通过 `Data` 类读取文件内容并用 `String` 类解码为字符串来输出。

## 深入了解

除了上面介绍的方法，我们还可以使用 `mkstemp(_:)` 函数来创建临时文件。这个函数会返回一个文件描述符和一个包含文件路径的C字符串。下面是一个示例：

```Swift
var buffer = [Int8](repeating: 0, count: Int(PATH_MAX))
let fileDescriptor = mkstemp(&buffer)
let path = String(cString: buffer)
```

这里使用了 `mkstemp(_:)` 函数来创建临时文件，并将返回的文件描述符和路径保存在 `fileDescriptor` 和 `path` 变量中。需要注意的是，我们需要手动将 `buffer` 转换为C字符串来获取文件路径。

## 参考链接

- [Apple官方文档 - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift八卦 - 如何操作临时文件？ | 非凡计算机](https://www.willson.net/post/create-temporary-file-in-swift/)

## 参考链接

- [Apple官方文档 - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift八卦 - 如何操作临时文件？ | 非凡计算机](https://www.willson.net/post/create-temporary-file-in-swift/)