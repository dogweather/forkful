---
title:    "Elixir: 开始一个新项目"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

##为什么

在开始新的项目时，使用Elixir编程语言有许多好处。Elixir是一种现代化的函数式编程语言，具有可扩展性和稳定性。它还有一个活跃的社区，可以为开发者提供支持和资源。

##如何开始

首先，你需要安装Elixir编译器。我们建议使用包管理器来安装，例如Homebrew（MacOS）或Chocolatey（Windows）。在你的操作系统上安装Elixir后，你可以在终端中输入以下命令来创建一个新的Elixir项目：

```Elixir
mix new my_project
```

这将在当前目录创建一个名为“my_project”的新项目。你可以通过在项目的根目录运行以下命令来启动Elixir命令行：

```Elixir
iex -S mix
```

现在你可以开始编写Elixir代码并在命令行中交互式地运行它们。例如，你可以定义一个简单的函数来打印一条信息：

```Elixir
defmodule Hello do
  def print_message do
    IO.puts "你好，世界！"
  end
end
```

然后，你可以通过在命令行中输入以下命令来编译和运行该函数：

```Elixir
Hello.print_message
```

这将在控制台中打印出“你好，世界！”。你也可以在这里找到更多的Elixir编程示例：[Elixir编程示例](https://elixir-lang.org/getting-started/introduction.html)。

##深入了解

如果你需要更详细的指导和资源来开始使用Elixir编程，我们推荐阅读这些文章：

- [为什么要使用Elixir](https://medium.com/@TheIronicCurtain/why-use-elixir-ae0f7ed0c9cc)
- [如何使用Elixir编写Web应用程序](https://medium.com/@andreichernykh/5-steps-to-build-a-web-application-with-phoenix-and-elixir-71a96e39455d)
- [Elixir与并发编程](https://medium.com/engineering-bankex/elixir-in-parallel-myth-or-reality-b2af71e9033d)

##另请参阅

- [Elixir官方文档](https://elixir-lang.org/docs.html)
- [Elixir论坛](https://elixirforum.com/)
- [Elixir博客](https://elixir-lang.org/blog/)