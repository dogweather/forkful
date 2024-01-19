---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Go语言新项目:快速上手

## 什么 & 为什么?
开始一个新项目就是让你的难以具象的想法变得形象并可以展现出来。程序员之所以开始新的项目，最常见的理由是解决某种问题，或者仅仅是肆意创新。

## 如何实现:
Go语言有一个内置工具`go mod`用来创建并管理模块。以下是命令行的使用方式：

```Go
// 在你的工作目录中开始
mkdir myproject
cd myproject

// 初始化新的模块
go mod init github.com/yourusername/myproject
```
现在你已经在 `myproject` 目录中创建了一个新的Go模块。

## 深入探讨
- **历史背景:** Go的模块管理系统在Go1.11版开始出现，一种解决Go包的版本和依赖管理问题的方式。
- **其它选择:** 在过去，很多第三方工具如`dep`,`glide`等用来管理Go的依赖。但自从Go1.11以后，Go语言团队推荐使用 `go mod` 作为官方的模块处理方式。
- **实现细节:** `go mod init`创建一个新的`go.mod`文件，这个文件描述了模块路径和依赖关系。

## 推荐阅读
- [Go Modules Reference](https://golang.org/ref/mod) 是对Go模块的详细介绍。
- [Go入门指南](https://tour.golang.org/welcome/1)提供了对Go编程的全面介绍。
- 在你遇到具体问题时，StackOverflow上的[Go](https://stackoverflow.com/questions/tagged/go)标签往往有很多你需要的答案。