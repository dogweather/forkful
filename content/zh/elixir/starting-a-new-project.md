---
title:                "Elixir: 开始一个新项目"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 为什么选择Elixir来开始一个新项目?

Elixir是一种功能强大且可靠的编程语言，它结合了函数式编程的优雅和并发性能。它也是建立在Erlang虚拟机上的，这意味着它具有出色的容错性和可伸缩性。选择Elixir来开始一个新项目可以带来更高的效率和更好的质量。

## 如何开始使用Elixir?

首先，您需要安装Elixir和Erlang。然后，您可以使用命令行工具来创建一个新的Elixir项目：

```Elixir
mix new my_project
```

接下来，您可以使用任何喜欢的代码编辑器来编写您的代码，并使用`iex`命令来运行Elixir的交互式环境。

下面是一个简单的Elixir程序示例，它输出“Hello World！”：

```Elixir
defmodule Hello do
  def hello do
    IO.puts "Hello World!"
  end
end

Hello.hello
```

输出：

```
Hello World!
```

## 深入了解开始新项目

在开始一个新项目之前，您应该熟悉函数式编程概念和Elixir语言的基本特性。您也可以使用Elixir内置的Web框架Phoenix来构建Web应用程序。

您还可以加入Elixir社区，参与讨论和分享经验。Elixir社区非常友好且乐于助人，您可以从中学习并提升自己的编程能力。

# 参考资料

- [Elixir官方网站](https://elixir-lang.org/)
- [Elixir School](https://elixirschool.com/zh-hans/)
- [Elixir论坛](https://elixirforum.com/)
- [Phoenix框架官方文档](https://hexdocs.pm/phoenix/overview.html)

# 查看更多资源

您可以通过访问上述参考资料来了解更多关于Elixir的信息，也可以查看这篇博客文章：[为什么选择Elixir来构建Web应用程序？有什么好处？](https://blog.appsignal.com/2020/07/01/why-choose-elixir-for-building-web-applications.html)，深入了解Elixir的优势和使用场景。