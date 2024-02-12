---
title:                "启动新项目"
aliases:
- /zh/go/starting-a-new-project/
date:                  2024-02-03T18:09:34.536601-07:00
model:                 gpt-4-0125-preview
simple_title:         "启动新项目"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/starting-a-new-project.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Go 中启动一个新项目涉及到设置一个工作空间，并用必要的 Go 模块对其进行初始化。程序员这样做是为了组织代码，有效管理依赖关系，并促进构建过程。这对于创建可扩展且可维护的 Go 软件来说是至关重要的。

## 怎么做：

首先，通过在终端运行 `go version` 确保你已经安装了 Go。你应该会看到安装的 Go 版本作为输出。接下来，让我们开始一个新项目。导航到你的工作空间并运行：

```shell
mkdir hello-world
cd hello-world
```

这将创建一个新的目录并将你移至你的项目目录中。现在，初始化模块：

```shell
go mod init example.com/hello-world
```

将 `example.com/hello-world` 替换为你的模块路径。此命令在你的目录中创建一个 `go.mod` 文件，标志着一个新 Go 模块的开始。`go.mod` 可能看起来像这样：

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` 跟踪你的项目的依赖项。现在，创建一个 `main.go` 文件：

```shell
touch main.go
```

在你最喜欢的编辑器中打开 `main.go` 并添加以下代码来打印 “Hello, World!”：

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

要运行你的程序，回到终端并执行：

```shell
go run main.go
```

你应该看到：

```plaintext
Hello, World!
```

恭喜你！你刚刚开始了一个新的 Go 项目并运行了你的第一个 Go 程序。

## 深入探讨

以模块为标准进行依赖管理在 Go 生态系统中的引入是一个重大转变，官方在 Go 1.11 中采用了这一做法。在模块之前，Go 开发人员依赖 GOPATH 环境变量来管理依赖项，这种方式不够直观，往往导致了臭名昭著的“依赖地狱”。

模块提供了一种封装的方式来管理项目依赖、版本控制，并且是使 Go 项目更加自给自足和可移植的一步。每个模块指定其依赖项，Go 在 `go.mod` 文件中跟踪这些依赖项，简化了不同环境和开发阶段的依赖管理。

然而，值得注意的是，尽管 Go 模块现在是标准，一些遗留项目可能仍然使用 GOPATH。对于大多数新项目来说，模块提供了一个更简单有效的管理系统，但了解 GOPATH 对于维护或贡献到较旧的 Go 代码库可能会很方便。

在替代方案方面，尽管 Go 模块现在是事实标准，Go 社区过去曾经尝试过其他依赖管理工具，比如 `dep`。然而，这些大多已经被集成到 Go 工具链中的官方模块支持所取代。
