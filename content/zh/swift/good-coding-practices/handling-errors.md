---
date: 2024-01-26 00:58:05.450797-07:00
description: "\u5728Swift\u4E2D\u8FDB\u884C\u9519\u8BEF\u5904\u7406\u610F\u5473\u7740\
  \u9884\u89C1\u5E76\u5BF9\u4EE3\u7801\u8FD0\u884C\u65F6\u51FA\u73B0\u7684\u95EE\u9898\
  \u4F5C\u51FA\u54CD\u5E94\u3002\u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u63A7\
  \u5236\u6DF7\u4E71\u2014\u2014\u9632\u6B62\u5E94\u7528\u5D29\u6E83\u5E76\u4E3A\u7528\
  \u6237\u63D0\u4F9B\u6D41\u7545\u7684\u4F53\u9A8C\u3002"
lastmod: '2024-03-11T00:14:21.970026-06:00'
model: gpt-4-1106-preview
summary: "\u5728Swift\u4E2D\u8FDB\u884C\u9519\u8BEF\u5904\u7406\u610F\u5473\u7740\u9884\
  \u89C1\u5E76\u5BF9\u4EE3\u7801\u8FD0\u884C\u65F6\u51FA\u73B0\u7684\u95EE\u9898\u4F5C\
  \u51FA\u54CD\u5E94\u3002\u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u63A7\u5236\
  \u6DF7\u4E71\u2014\u2014\u9632\u6B62\u5E94\u7528\u5D29\u6E83\u5E76\u4E3A\u7528\u6237\
  \u63D0\u4F9B\u6D41\u7545\u7684\u4F53\u9A8C\u3002"
title: "\u5904\u7406\u9519\u8BEF"
---

{{< edit_this_page >}}

## 何为错误处理？为何要做？
在Swift中进行错误处理意味着预见并对代码运行时出现的问题作出响应。我们这样做是为了控制混乱——防止应用崩溃并为用户提供流畅的体验。

## 怎样进行错误处理：
Swift 使用 `do`、`try` 和 `catch` 代码块来进行错误处理。让我们来看一下：

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // 假设我们在这里有一些逻辑来检查文件是否存在以及我们是否有权限读取它
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "文件内容在这里"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("糟糕！文件未找到。")
} catch FileError.noPermission {
    print("啊！没有权限读取文件。")
} catch {
    print("发生了未知错误。")
}
```

示例输出：

```
糟糕！文件未找到。
```

## 深入理解
错误处理并不总是像现在这样顺畅。在Objective-C中，你要处理指向NSError对象的指针，这感觉有些笨拙。现在，我们有了一个更优雅的系统，使用Swift枚举和 `Error` 协议。

Swift 的 `throw` 让我们能够提示有些事情变得不正常。`do` 代码块表现得像是意识到错误的领域，`try` 前缀调用那些风险操作，而 `catch` 负责处理出现的问题。

可选项（Optionals）是一种针对处于“错误”状态的情况的替代品，但可能仍然没有“结果”。它们有点像薛定谔的变量——要么有值，要么没有。

要真正深入了解，请查阅 `Result` 类型，它们是常规返回和错误模式之间的时髦混合体。

## 参见
- 官方Swift错误处理指南：[Apple 文档](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Swift错误处理最佳实践：[RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Swift中的高级错误处理：[Medium 文章](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
