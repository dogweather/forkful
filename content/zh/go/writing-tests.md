---
title:    "Go: 编写测试"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么写测试？

很多开发人员都觉得写测试很繁琐，浪费时间。但实际上，写测试对于保证代码的质量和可靠性至关重要。它可以帮助我们在开发过程中发现潜在的问题，提高代码的稳定性和可维护性。因此，学习如何写测试是每个程序员都应该掌握的技能。

## 如何编写测试

Go语言内置了一个非常强大的测试框架，让我们可以轻松编写和运行测试。下面的示例将演示如何使用Go来编写一个简单的测试。

```Go
// main.go

package main

import "fmt"

// Add函数，用于返回两个整数的和
func Add(x, y int) int {
	return x + y
}

func main() {
	fmt.Println(Add(2, 3))
	fmt.Println(Add(-5, 10))
}
```

```Go
// main_test.go

package main

import "testing"

// 测试Add函数
func TestAdd(t *testing.T) {
	result := Add(2, 3)
	if result != 5 {
		t.Errorf("Add(2, 3) expected to be 5, got %d", result)
	}
}
```

运行测试：

```
$ go test
```

输出结果：

```
$ go test
PASS
ok      _/Users/username/Go/src/project       0.005s
```

我们可以看到，测试通过了。如果我们把Add函数中的加号改成减号，那么测试就不会通过，从而可以发现我们的代码存在问题。

## 深入了解测试

除了基础的单元测试外，Go还提供了一些其他类型的测试，比如性能测试和压力测试。同时，Go还支持用表格驱动测试的方式来简化测试代码。除此之外，我们还可以使用断言等技巧来使测试代码更加简洁和有意义。

## 参考资料

- [Go语言官方文档：测试](https://golang.org/pkg/testing)
- [Go语言测试入门](https://www.jianshu.com/p/795db8cbf25c)
- [深入理解Go语言中的测试技术](https://www.cnblogs.com/slmmb/p/8682602.html)

# 参见

- [Go语言官方文档](https://golang.org/)
- [Go语言中文网](http://docscn.studygolang.com/)