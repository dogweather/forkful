---
title:                "处理错误"
date:                  2024-01-26T00:54:51.918823-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
错误处理就是您的代码在执行过程中出现问题时应对策略的体现——就像在不掉球的情况下接住一个曲球。程序员这样做是为了防止程序崩溃，给用户提供流畅的体验。

## 如何操作：
Kotlin 提供了 `try`、`catch`、`finally` 和 `throw` 来管理错误。以下是它们的使用方式：

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0
    
    try {
        val result = numerator / denominator
        println("结果：$result")
    } catch (e: ArithmeticException) {
        println("兄弟，不能除以零。")
    } finally {
        println("无论如何这都会发生。")
    }
}
```

输出：
```
兄弟，不能除以零。
无论如何这都会发生。
```

如果 `try` 块中出现问题，执行会立即跳到 `catch` 块。它捕获抛出的特定错误（在这个例子中是 `ArithmeticException`）。无论结果如何，`finally` 块都会执行。

## 深入了解
`try-catch` 块自编程早期以来就一直是一项功能——它就像是一个安全网。Kotlin 还提供了 `throw` 关键字，用于手工抛出异常到程序中，而 `finally` 用于必须执行的代码——通常是清理工作。

其他选择包括 `Result` 类型和将 Kotlin 的 `try` 作为表达式使用。

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
这种方法返回一个 `Result` 对象——您要么得到成功，要么得到失败，而无需处理未处理的异常的麻烦。

Kotlin 的实现非常整洁，因为您可以将 `try` 用作表达式，这意味着它会返回一个值。这样的选择使得 Kotlin 的错误处理非常灵活。这就像在车间选择合适的工具一样，关键在于选对适合工作的工具。

## 参见
- Kotlin 异常处理文档：[Kotlin 异常处理](https://kotlinlang.org/docs/exception-handling.html)
- Kotlin `Result` 类型文档：[Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- 《Effective Java》，第3版，作者是 Joshua Bloch——对异常有很好的见解，尽管它是针对 Java 的。
