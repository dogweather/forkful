---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么?)
编写测试是检查代码能否如期运行的过程。程序员之所以要编写测试，是因为测试可以提前发现错误，保证代码质量。

## How to: (如何做：)
在Go中，测试是通过*_test.go文件来进行的。我们使用`testing`包来编写测试用例。

```Go
package main

import (
    "testing"
    "reflect"
)

// 要测试的函数
func Add(a, b int) int {
    return a + b
}

// 测试函数以Test开头，参数为*testing.T
func TestAdd(t *testing.T) {
    got := Add(2, 3)
    want := 5

    if got != want {
        t.Errorf("Add(2, 3) = %d; want %d", got, want)
    }
}
```

运行`go test`命令，得到以下输出：

```
PASS
ok  	example.com/yourpackage	0.165s
```

## Deep Dive (深入探究)
Go语言自1.0版本起便内置了测试框架。与其他语言如Python中的pytest或JavaScript中的Jest不同，Go的`testing`包简洁直接，无需安装额外库。实现细节上，Go的测试利用反射(reflect)和接口(interface)提供灵活的测试方法。

## See Also (另请参阅)
- Go官方测试文档: [https://golang.org/pkg/testing/](https://golang.org/pkg/testing/)
- Go测试代码示例: [https://github.com/golang/go/wiki/TableDrivenTests](https://github.com/golang/go/wiki/TableDrivenTests)
