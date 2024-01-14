---
title:                "Swift: 写到标准错误"
simple_title:         "写到标准错误"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么：为什么会有人使用标准错误的写入？

作为一个Swift开发者，你可能已经发现有时候你的代码需要写入错误信息到标准错误流中。那么为什么需要这样做呢？通常情况下，我们会使用标准输出来输出我们的程序运行结果，但是标准错误可以用来输出一些错误相关的信息，比如程序崩溃的错误信息。这样我们在调试程序时可以更快速地定位问题所在。

## 如何使用标准错误输出

为了向标准错误输出信息，我们可以使用Swift语言内置的print函数，并将要输出的信息作为参数传入。例如：

```Swift
print("出现了一个错误：文件不存在", to: &stderr)
```

这段代码会将“出现了一个错误：文件不存在”这条信息输出到标准错误流中。

我们也可以通过自定义一个错误类型，并将其作为print函数的参数来输出自定义的错误信息。代码示例如下：

```Swift
enum NetworkError: Error {
  case connectionFailed
  case requestTimeout
  case serverError 
}

let error = NetworkError.connectionFailed
print(error, to: &stderr)
```

这将输出“connectionFailed”到标准错误流中。需要注意的是，我们需要将错误信息打印到标准错误流中，而不是标准输出流。

## 深入了解标准错误输出

在Swift中，标准错误流是一个**文件指针**（File Pointer），它与普通的文件指针不同的是，它指向了标准错误流（stderr）的文件句柄。这意味着我们可以通过更低级的文件操作来控制标准错误流。

例如，我们可以通过打开标准错误流所指向的文件来获取文件描述符（File Descriptor），这是一个用来标识文件的整型值。通过这个文件描述符，我们可以对标准错误流进行更多的操作，比如改变它的指向，或者关闭它。

```Swift
// 打开标准错误流所指向的文件，获取文件描述符
let fileDescriptor = stderrPtr.pointee.writePrt
// 通过文件描述符来对标准错误流进行操作
close(writePrt)
```

另外，标准错误流也可以被重定向到其他文件，或者被关闭。这样做的好处是我们可以在程序中自定义标准错误流的输出，从而更好地记录错误信息。

## 参考链接

- [Swift Standard Library Documentation - Standard Error](https://developer.apple.com/documentation/swift/standardstream/standarderror)
- [Basics of Writing to Standard Error](https://www.mokacoding.com/blog/say-goodbye-to-print-and-hello-to-nslog/#basics-of-writing-to-standarderror)

# 参考资料

[查看其他标准流的使用方法](https://developer.apple.com/documentation/swift/standardstream)

如果你想了解更多有关标准错误输出的使用方法，可以参考以上资料。通过学习如何使用标准错误输出，我们可以更有效地调试我们的程序，提高开发效率。记得在适当的时候使用标准错误输出，这将帮助你更快地找到程序中的错误。