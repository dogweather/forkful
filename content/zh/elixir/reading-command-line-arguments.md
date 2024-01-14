---
title:                "Elixir: 读命令行参数"
simple_title:         "读命令行参数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么
要了解如何阅读命令行参数，是因为它能够让你更有效地编写 Elixir 程序，尤其是当你需要访问用户输入或配置程序时。

## 如何做
通过以下的示例代码，你将学会如何在 Elixir 中读取命令行参数，并且输出它们的值。

```Elixir
# 获取命令行参数
args = System.argv
# 输出参数列表
IO.inspect(args)

# 获取第一个参数（通常为程序名称）
first_arg = System.argv[0]
# 输出第一个参数
IO.puts(first_arg)

# 获取第二个参数（如果存在）
second_arg = System.argv[1]
# 输出第二个参数
IO.puts(second_arg)
```

在上面的示例中，我们使用了 `System.argv` 函数来获取命令行参数，然后使用 `IO.inspect` 和 `IO.puts` 函数来输出参数的值。你可以根据自己的需要来读取更多的参数。

## 深入了解
除了上面提到的方法，你还可以使用 `OptionParser` 模块来读取命令行参数，并定义参数的类型以及默认值。下面是一个例子：

```Elixir
# 引入 OptionParser 模块
import OptionParser

# 定义命令行参数的选项
options = [switches: [verbose: :boolean]]

# 使用 OptionParser 解析命令行参数
parsed_args = parse(options)

# 输出 verbose 参数的值
IO.puts(parsed_args[:verbose])
```

除了布尔类型外，你还可以定义整数、字符串、列表等类型的参数，并为它们设置默认值。想要了解更详细的用法，请阅读 Elixir 官方文档中有关 `OptionParser` 的部分。

## 参考链接
- 官方文档中有关[命令行参数的章节](https://elixir-lang.org/getting-started/mix-otp/command-line.html)
- 开源项目中关于 [OptionParser 的使用](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/option_parser.ex) 的代码实现
- [Elixir Forum](https://elixirforum.com/) 上有关阅读命令行参数的讨论帖子。

# 查看也可以
- [Elixir 在命令行下的使用指南](https://www.eastfive.opengl.cn/elixir-on-the-command-line/)
- [Elixir 101: Command-line Applications with Elixir](https://dev.to/christianbaroni/elixir-101-command-line-applications-with-elixir-1pjg)