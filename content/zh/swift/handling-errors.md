---
title:                "处理错误"
date:                  2024-01-26T00:58:05.450797-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/handling-errors.md"
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
