---
title:                "处理错误"
date:                  2024-01-26T00:52:50.343640-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/handling-errors.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

Go 语言中的错误处理是关于优雅地捕获和响应运行时的小问题。我们这么做是为了防止程序崩溃，并确保我们的程序即使在出现意外情况时也能预测地执行。

## 如何操作：

Go 使用显式错误处理。这意味着你每次调用一个函数时，都要检查是否返回了错误。没有异常。以下是它的样子：

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("哎哟:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// 假装出了点问题
	return fmt.Errorf("出了点问题")
}
```

运行这段代码，你会得到：

```
哎哟: 出了点问题
```

但如果它成功了呢？

```Go
func doSomething() error {
	// 这次一切正常
	return nil
}
```

没有输出。酷，没有消息就是好消息。

## 深入探究：

在 Go 中，错误处理一直是争议的焦点。Go 从一开始就决定不使用异常，而是采取更明确的方法，这种方法有些开发者因其简洁而喜欢，但其他人则觉得它过于冗长。内置的 `error` 类型是一个接口。任何带有 `Error() string` 方法的类型都能满足它。这与 Go 简洁明了的精神相契合。

其他选择？有 `panic` 和 `recover` 这一对，但它们用于特殊情况（不是开玩笑的）当程序无法继续时。可以将 `panic` 视为你知道没有回头路时按下的弹射按钮。尽量少用它。

至于主流的错误处理，Go 1.13 引入了错误包装，通过诸如 `errors.Is()` 和 `errors.As()` 这样的函数更容易地理解“错误链”。

## 另请参见：

关于 Go 中所有错误处理的内容：

- Go 博客关于错误处理：[https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- 《高效 Go 编程》— 错误处理部分：[https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 错误包装文档：[https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheney 关于错误处理策略的文章：[https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)