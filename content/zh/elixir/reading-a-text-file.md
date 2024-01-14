---
title:    "Elixir: 读取文本文件。"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 为什么阅读文本文件？
阅读文本文件是日常中不可或缺的一部分，特别是在编程领域。无论是查看配置文件、读取数据或者调试错误，阅读文本文件都是必要的。在Elixir编程语言中，我们可以使用内置的函数来轻松读取文本文件。

## 如何读取文本文件
要读取文本文件，我们可以使用Elixir的`File`和`IO`模块提供的函数。首先，我们需要打开文件并将其存储在变量中，如下所示：
```
file = File.open("sample.txt")
```
然后，我们可以使用`IO.read/2`函数来一次性读取整个文件的内容：
```
contents = IO.read(file, :all)
```
我们也可以使用`IO.readline/1`函数每次读取一行内容，直到文件结束：
```
loop do
  line = IO.readline(file, chomp: true)
  IO.puts(line)
end
```
在以上示例中，我们使用了`loop`循环和`IO.puts/1`函数来打印每一行的内容。当文件结束时，`IO.readline/1`函数将抛出一个`EOFError`，我们可以捕获它来结束循环。

## 深入了解
在Elixir中，读取文本文件的默认编码是UTF-8。如果需要，我们也可以在读取函数中指定其他编码，例如：
```
contents = IO.read(file, :all, encoding: "latin1")
```
此外，`IO.read/2`函数还接受一个`chunk_size`参数，用于限制每次读取的字节数。这在处理大型文件时非常有用。

另外，我们也可以使用`File.stream!/1`函数来创建一个文件流，它可以更有效地读取大型文件，特别是当我们只需要处理一部分内容时。

# 请参阅
- [Elixir文档：File模块](https://hexdocs.pm/elixir/File.html)
- [Elixir文档：IO模块](https://hexdocs.pm/elixir/IO.html)
- [Elixir文档：File流](https://hexdocs.pm/elixir/File.html#stream!/1)