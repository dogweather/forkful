---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:14:54.238863-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
REPL（读取-求值-打印循环）能让你实时与代码互动；它读取输入，评估它，打印结果，然后循环返回。程序员使用它来测试代码片段、调试、以及实时学习新语言。

## 如何操作：
Go没有内置REPL，但你可以使用第三方工具。一个受欢迎的工具是`gore`:

```go
// 使用以下命令安装gore
$ go install github.com/motemen/gore/cmd/gore@latest

// 运行gore
$ gore
gore版本0.5.0  :help 获得帮助
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## 深入了解
REPL最初为Lisp开发，现在在Python或Ruby等动态语言中很常见。Go作为一种静态类型语言，没有自带的REPL。`gore`的替代品包括`go-pry`和`yaegi`。这些工具解释Go代码，让你能够快速探索和验证想法，无需编译一个完整的应用。它们对初学者和教育环境特别有用，其中的重点是学习和实验。

## 另请参见
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
