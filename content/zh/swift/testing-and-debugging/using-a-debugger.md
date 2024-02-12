---
title:                "使用调试器"
aliases:
- /zh/swift/using-a-debugger.md
date:                  2024-01-26T04:10:50.206772-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么与为什么？
使用调试器意味着利用专业工具来测试和检查你的代码在运行时的情况。这很重要，因为它能让你看到代码内部发生了什么，找到错误，并更好地理解代码的行为。

## 如何操作：
要在Xcode（Swift的集成开发环境）中使用调试器，你可以设置断点、检查变量和观察表达式。这里有一个例子：

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

通过在Xcode中一个行号的左侧点击设置一个断点，然后运行程序。当程序遇到断点时，Xcode会暂停执行。现在你可以：

1. 检查变量的值。
2. 使用调试器控件进行逐过（执行下一行）或跳入（进入一个函数）。
3. 向“监视列表”中添加表达式，以监视特定变量或常量的更改。

在调试区域你可能会看到这样的内容：

```
(lldb) po number
5
(lldb) po result
120
```

## 深入探讨：
调试器自1940年代以来一直是编程领域的一部分，从简单的断点系统演变到复杂的、UI驱动的体验。除了Xcode内置的调试器，其他选项包括第三方工具，比如Xcode在底层使用的LLDB（低级调试器）。有些人甚至使用`print()`语句进行调试（亲切地称为“原始调试”），但对于大型项目或复杂错误来说，这种方法效率较低。当你使用调试器时，你在处理执行控制、运行时内省和数据操作。深刻理解这些原则对于高效调试来说非常重要。

## 另请参阅：
- [苹果的Xcode调试指南](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDB快速入门指南](https://lldb.llvm.org/use/tutorial.html)
- [Ray Wenderlich的Swift调试教程](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
