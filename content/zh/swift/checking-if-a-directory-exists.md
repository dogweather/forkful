---
title:    "Swift: 检查目录是否存在"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

当我们在编写Swift程序时，我们经常会涉及到文件和文件夹的操作，而有时候我们需要判断一个文件夹是否存在。如果我们没有进行此项检查，可能会导致程序出错或运行不正常。因此，检查文件夹是否存在对于保证程序的健壮性和稳定性非常重要。

## 如何进行检查

在Swift中，我们可以通过使用FileManager类来检查文件夹是否存在。我们可以使用该类的方法`fileExists(atPath:)`来判断指定路径上的文件是否存在。下面是一个简单的例子：

```Swift
let fileManager = FileManager.default
let directoryPath = "path to your directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("文件夹存在")
    } else {
        print("指定路径上存在一个文件，不是文件夹")
    }
} else {
    print("文件夹不存在")
}
```

在上面的代码中，我们首先创建了一个`FileManager`实例，然后指定了要检查的文件夹的路径。通过调用`fileExists(atPath:)`方法，我们可以得到一个布尔值来表示文件夹是否存在。同时，我们还使用了传入的`isDirectory`参数来判断指定路径上的文件是否为文件夹。最后，根据不同的情况，打印出相应的提示信息。

## 深入探讨

在深入探讨之前，我们需要了解一下`fileExists(atPath:)`方法的返回值类型。它的返回值类型为`Bool`，表示文件或文件夹是否存在。而`isDirectory`参数的类型为`ObjCBool`，它是一个Objective-C类型，但是可以在Swift中使用。这个参数的作用是告诉我们指定路径上的文件是否为文件夹，而不仅仅是存在与否。

另外，需要注意的是，在使用`fileExists(atPath:)`方法时，我们传入的路径必须是绝对路径，而不是相对路径。因此，在指定路径时需要特别注意。

## 查看更多

如果您想了解更多有关Swift中检查文件夹是否存在的知识，可以参考以下链接：

- [Swift中FileManager类的官方文档](https://developer.apple.com/documentation/foundation/filemanager)
- [NSHipster：FileManager](https://nshipster.com/filemanager/)
- [Swift中的路径操作和FileManager](https://www.hackingwithswift.com/read/12/overview)
- [Swift文档中关于FileManager类的详细介绍](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)