---
title:                "打印调试输出"
html_title:           "Swift: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么要打印调试输出

在编程中，出现bug是很常见的事情。为了找到问题所在，打印调试输出是一种有效的方法。它可以帮助开发者跟踪程序运行过程中的变量值，帮助发现潜在的错误。

## 如何打印调试输出

打印调试输出的方法很简单，Swift提供了一个内置函数print来实现。在需要打印的位置使用print函数，并将需要跟踪的变量作为参数传入即可。例如：

```Swift
let num = 10
print(num)
```

这段代码会打印出变量`num`的值，即`10`。如果想要打印多个变量，可以用逗号隔开，例如：

```Swift
let num1 = 10
let num2 = 20
print(num1, num2)
```

这样就会依次打印出`10`和`20`。除了基本数据类型，也可以打印字符串和数组等复杂数据类型。下面是一个更复杂的示例：

```Swift
let name = "John"
let age = 25
let hobbies = ["reading", "hiking", "coding"]
print("My name is \(name), I am \(age) years old. My hobbies are \(hobbies)")
```

运行后，会得到如下输出：

```
My name is John, I am 25 years old. My hobbies are ["reading", "hiking", "coding"]
```

## 深入探讨打印调试输出

除了基本的print函数，Swift还提供了更多的调试输出方式，包括`debugPrint`、`dump`和`NSLog`等。它们分别有些不同，可以根据不同的需求选择使用。此外，可以通过设置`#if DEBUG`条件来控制是否打印调试输出，这在发布版本时可以避免将调试信息暴露给用户。

## 参考链接

- [Apple官方文档：Debugging with Print Statements](https://developer.apple.com/documentation/swift/debugging_with_print_statements)
- [简书：Swift 调试输出的种种姿势](https://www.jianshu.com/p/fcb4b5a06247)
- [知乎：Swift中的print 和 NSLog有何区别？](https://www.zhihu.com/question/51380822)

## 参见

- [Swift官方文档](https://swift.org)
- [Swift中文社区](https://swift.gg)