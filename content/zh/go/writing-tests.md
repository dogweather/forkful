---
title:                "Go: 作岀測試 (Zuò chū cè shì)"
simple_title:         "作岀測試 (Zuò chū cè shì)"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么写测试？

写测试是保证代码质量的重要步骤。通过编写测试，我们可以更容易地发现代码中的 bug，并确保代码的稳定性和可靠性。同时，测试也可以帮助开发人员更好地理解代码，并提高代码的可读性和可维护性。

## 如何编写测试

完成 Go 语言的测试非常容易，只需要遵循以下几个步骤：

1. 创建一个名为 `xxxx_test.go` 的文件，其中 `xxxx` 为需要进行测试的文件名或函数名。

2. 使用 `import "testing"` 导入 Go 的测试包。

3. 在 `xxxx_test.go` 文件中，使用 `func TestXxxx(t *testing.T)` 进行定义测试函数。其中 `Xxxx` 为需要进行测试的函数名。

4. 在测试函数中，使用 `t.Run()` 和 `t.Error()` 或 `t.Fatal()` 来运行和检查测试结果。

以下是一个例子（假设我们需要测试一个名为 `Add` 的函数）：

```Go
package main

import (
	"testing"
)

func TestAdd(t *testing.T) {
	result := Add(2, 3)
	if result != 5 {
		t.Error("Expected 5, got", result)
	} else {
		t.Log("Success!")
	}
}
```

运行以上测试函数，如果结果正确，会输出 `Success!`，如果结果不正确，会输出 `Error` 。

## 深入了解写测试

除了简单的检查结果外，Go 还允许我们进行更深层次的测试。例如，可以使用 `t.Helper()` 来标记为辅助函数，将测试内容显示为更详细的文本。此外，还可以使用 `t.Skip()` 、 `t.SkipNow()` 和 `t.Skipf()` 来跳过某些测试。

## 参考链接

- [Go 语言官方文档：testing 包](https://golang.org/pkg/testing/)

- [Go 语言测试最佳实践](https://medium.com/@tdenev91/testing-in-golang-best-practices-yield-examples-1acebda6400)

- [Go 语言测试详解](https://developer.ibm.com/zh/articles/au-golang-testing/)