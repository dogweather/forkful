---
title:    "Swift: 向标准错误输出数据"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么
有时候，在编程的过程中，我们会遇到一些错误。而这些错误可能会对我们的程序造成严重的影响。为了更好地进行调试和定位错误，我们可以选择将错误信息输出到标准错误流（standard error）。这样，我们就可以更有效率地解决问题，加快程序的开发进程。

## 如何做
如果我们想要将错误信息输出到标准错误流，可以使用Swift语言中的```print(_:to:)```方法。下面是一个简单的示例代码：

```Swift
let errorMessage = "出现了一个错误！"
print(errorMessage, to: &standardErrorStream)
```

上述代码中，我们通过将错误信息传递给```print(_:to:)```方法的第二个参数，即标准错误流的地址，来将错误信息输出到标准错误流中。

## 深入探究
标准错误流（standard error）通常是与标准输出流（standard output）相互独立的。这意味着，即使将错误信息输出到标准错误流，标准输出流仍然会正常输出。此外，与标准输出流不同的是，标准错误流的内容会被打印为红色，以便我们更容易地区分。

除了使用```print(_:to:)```方法外，我们也可以使用```FileHandle.standardError```来获取标准错误流的实例，然后调用```write(_:Data)```方法来写入错误信息。但需要注意的是，调用```write(_:Data)```方法时，需要将错误信息转换为Data类型。

最后，值得一提的是，我们也可以使用标准错误流来输出一些警告信息，而不仅仅是错误信息。

## 参考资料
- [Swift官方文档](https://docs.swift.org/swift-book/LanguageGuide/Errors.html)
- [Using stderr in Swift](https://stackoverflow.com/questions/24802606/using-stderr-in-swift)

## 参见
- [输出到标准输出流（standard output）](https://www.example.com/standard-output)
- [使用Swift语言进行调试](https://www.example.com/swift-debugging)