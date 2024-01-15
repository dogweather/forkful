---
title:                "写入标准错误"
html_title:           "Swift: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

你可能会问，为什么我们需要写入标准错误流？答案很简单，标准错误流是一种重要的输出流，可以帮助我们在调试和排查错误时更轻松地定位问题。在编写Swift代码时，我们可能会遇到各种错误，使用标准错误流可以帮助我们及时发现并解决这些问题。

## 如何

如果你想要使用Swift写入标准错误流，可以按照以下步骤进行操作：

1. 首先，我们需要导入`Foundation`框架，该框架提供了许多与输入输出相关的功能。
2. 然后，我们可以使用`FileHandle.standardError`来获取标准错误流的句柄。
3. 使用`write(_:Data)`方法将错误信息写入标准错误流，其中`Data`参数为要写入的数据。
4. 最后，记得在错误信息末尾添加换行符`\n`，以便在终端中显示为单独的一行。完整的代码示例如下：

```Swift
import Foundation

let errorMessage = "出错了！"
if let data = errorMessage.data(using: .utf8) {
    FileHandle.standardError.write(data)
    FileHandle.standardError.write("\n".data(using: .utf8)!)
}
```

运行以上代码，我们可以在终端中看到打印出的错误信息：`出错了！`

## 深入了解

除了手动写入标准错误流之外，Swift还提供了方便的`print(_:to:separator:terminator:)`方法来直接将内容打印到标准错误流。该方法接受四个参数，分别为要打印的内容、目标输出流、分隔符和结尾标记。其中，目标输出流为`&stderr`，表示将内容打印到标准错误流。示例如下：

```Swift
let errorMessage = "出错了！"
print(errorMessage, to: &stderr)
```

除此之外，我们还可以使用标准错误流来记录和保存错误日志，方便我们在之后进行查看和分析。使用`FileHandle`类的`seekToEndOfFile()`方法可以将光标定位到文件末尾，接着使用`write(_:Data)`方法将错误日志写入文件。示例如下：

```Swift
let errorLogPath = "/Users/UserName/ErrorLog.txt"
let errorMessage = "出错了！"

// 记录错误日志
if let fileHandle = try? FileHandle(forWritingTo: URL(fileURLWithPath: errorLogPath)) {
    fileHandle.seekToEndOfFile()
    let log = "[\(Date())] \(errorMessage)\n"
    if let data = log.data(using: .utf8) {
        fileHandle.write(data)
    }
}

// 读取错误日志
if let data = try? Data(contentsOf: URL(fileURLWithPath: errorLogPath)) {
    if let errorLog = String(data: data, encoding: .utf8) {
        print(errorLog)
    }
}
```

## 参考链接

- [Swift官方文档](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Apple Developer Documentation](https://developer.apple.com/documentation/swift/errorhandling)
- [Swift中文版](https://swiftgg.gitbook.io/swift/)

直到这里，我们已经了解了如何使用Swift写入标准错误流，并且可以根据自己的需求来灵活运用。希望本文能够帮助你更轻松地处理Swift代码中的错误！

## 参见

- [使用Swift写入标准输出流的方法](https://example.com)
- [Swift中的异常处理](https://example.com)