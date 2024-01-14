---
title:    "Swift: 读取命令行参数"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 为什么

命令行参数是Swift编程中一个常用的概念，它可以为程序提供用户输入的数据。通过学习如何读取命令行参数，你可以更加灵活和高效地使用Swift来编写程序。

## 如何做

首先，让我们来看一个简单的例子。假设我们需要一个程序，可以打印出用户输入的名字和年龄。我们首先可以使用`CommandLine`类来获取命令行参数，然后使用`print()`函数来打印输出。

```Swift
let name = CommandLine.arguments[1]
let age = CommandLine.arguments[2]
print("\(name) is \(age) years old.")
```

接下来，我们可以在终端中输入命令行参数来测试这段代码的输出。假设我们的程序名为`Hello`，我们可以输入以下命令来运行程序：

```
Hello John 25
```

这样程序就会输出`John is 25 years old.`。我们也可以通过修改`CommandLine.arguments`中的索引来读取不同位置的参数，以实现更多的功能。

## 深入了解

除了简单地读取参数外，我们还可以通过一些技巧来优化我们的代码。这包括使用`optional`类型来避免无效的参数输入、使用`contains()`函数来检查参数是否存在以及使用`Int()`或`Double()`来将参数转换成数字类型等等。

同时，我们也可以通过查阅官方文档了解更多关于`CommandLine`类的方法和属性，以及如何使用它们来处理命令行参数。

## See Also

- [Apple官方文档：CommandLine类](https://developer.apple.com/documentation/foundation/commandline)
- [Swift开发者社区：命令行参数入门指南](https://www.jianshu.com/p/d243d8edb8ca)
- [CocoaChina：Swift命令行参数解析](https://blog.cocoachina.com/article/64232)