---
title:    "Elixir: 创建临时文件"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么要创建临时文件？

在编程中，我们经常会遇到需要临时存储数据的情况。比如，当我们需要下载一个网络资源时，我们可能需要先将它保存到一个临时文件中，然后再处理它。创建临时文件可以帮助我们有效地管理这些临时数据，并保证它们不会占用过多的存储空间。

## 如何创建临时文件

```Elixir
File.temp_file!("tmp", "test")
```

这个函数将在系统的临时目录中创建一个名为 "tmp" 的文件夹，并在其中生成一个名为 "test" 的临时文件。我们可以在创建临时文件时指定文件夹的路径和文件名。临时文件默认会在程序结束时自动删除，但我们也可以通过设置 `persist:true` 来保留临时文件。

下面是一个使用临时文件的简单示例：

```Elixir
# 创建临时文件
file = File.temp_file!("tmp", "test")

# 向临时文件写入数据
File.write(file, "This is a test file.")

# 从临时文件中读取数据
contents = File.read(file)

# 打印输出
IO.puts(contents)

# 删除临时文件
File.rm(file)
```

输出：

```
This is a test file.
```

## 深入了解创建临时文件

临时文件的命名规则是由 `File.temp_file!/2` 函数决定的。默认情况下，它会将文件名的前缀和后缀添加到随机生成的字符串中，以确保文件名的唯一性。如果有需要，我们也可以在函数中传入自定义的前缀和后缀。

在创建临时文件时，我们还可以选择指定文件的访问权限。默认情况下，临时文件的权限为 `:private`，但我们可以通过设置 `access` 参数来修改。

总的来说，创建临时文件是一个非常简单又实用的功能。它可以帮助我们更好地管理临时数据，并提高程序的执行效率。

## 参考资料

- [Elixir官方文档：创建临时文件](https://hexdocs.pm/elixir/File.html#temp_file!/2)
- [掘金：如何在Elixir中创建临时文件](https://juejin.im/post/6867929546431542285)
- [知乎：如何在Elixir中使用临时文件进行文件IO操作](https://zhuanlan.zhihu.com/p/345915547)

## 参见

- [Elixir官方文档](https://elixir-lang.org/)
- [Elixir中国社区](https://elixir-cn.com/)
- [Elixir编程语言论坛](http://forum.phoenixchina.org/)