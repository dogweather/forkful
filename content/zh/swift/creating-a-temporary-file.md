---
title:    "Swift: 创建临时文件"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 为什么

临时文件是计算机编程中常见的概念，它们可以帮助我们在程序中存储和处理临时数据。创建临时文件可以帮助我们提高程序的性能和稳定性，因为它们可以在程序执行完毕后自动被删除。

## 如何创建临时文件

在Swift中，我们可以使用`FileManager`类来创建临时文件，并且使用`createTemporaryFile()`方法来指定文件名和内容。下面是一个简单的示例代码和输出：

```Swift
// 导入FileManager库
import Foundation

// 创建FileManager对象
let fileManager = FileManager.default

// 创建临时文件
let tempFile = fileManager.createTemporaryFile(filename: "myTempFile", content: "This is a temporary file created for demonstration purposes.")

// 打印临时文件的路径
print("Temporary file path: \(tempFile.path)")

// 输出: Temporary file path: /var/folders/g4/7dh1bx1j2hb2b214i2q12_000000gp/T/myTempFile.tmp
```

## 深入了解

我们可能会想知道，临时文件是如何被创建和删除的。实际上，它们是在计算机的临时目录中被创建的，这通常是一个系统保留的文件夹，在程序执行结束后会自动被删除。对于不同的操作系统，临时目录的位置可能会有所不同。

此外，我们还可以通过设置文件的相关属性来控制临时文件的生命周期。比如，我们可以设置它们的生存期为程序运行结束后的一定时间，或者在程序执行过程中保留临时文件以供调试。

## 查看亦可

[sampleCode1](https://github.com/swift-code-examples/FileManager#create-temporary-file-in-swift) <br>
[sampleCode2](https://swift.gg/2016/11/07/make-temporary-files-in-swift/) <br>
[官方文档](https://developer.apple.com/documentation/foundation/nsfilemanager/1414861-createtemporaryfile)