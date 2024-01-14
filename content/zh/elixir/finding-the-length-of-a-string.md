---
title:                "Elixir: 寻找字符串的长度"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##为什么
有时候，在编程中我们需要知道一个字符串的长度。这可以帮助我们处理文本、验证用户输入或者进行排序。在Elixir中，有几种方法可以轻松地找到字符串的长度。

##如何
在Elixir中，我们可以使用`String.length`函数来获取一个字符串的长度。让我们来看一个简单的例子：

```Elixir
string = "Hello"
IO.puts String.length(string)
```

输出将是： 5

另一种获得字符串长度的方法是使用`String.trim_leading`函数。这个函数将返回一个经过修剪后的字符串，然后我们可以使用`String.length`函数来获取修剪后的字符串的长度。让我们看一个例子：

```Elixir
string = "  Hello  "
trimmed_string = String.trim_leading(string)
IO.puts String.length(trimmed_string)
```

输出将是：5

##深入探究
在Elixir中，字符串实际上是一个由字符列表组成的列表。当我们使用`String.length`函数时，它会遍历字符串并计算字符的数量，最终返回字符串的长度。因此，它的运行时间是O(n)，其中n是字符串的长度。

除了使用`String.length`函数外，我们还可以自己实现一个函数来计算字符串的长度。让我们来看一个简单的例子：

```Elixir
defmodule MyString do
  def get_length(string) do
    length = 0
    Enum.each(string, fn character ->
      length = length + 1
    end)
    length
  end
end

string = "Hello"
IO.puts MyString.get_length(string)
```

输出将是：5

在这个例子中，我们使用`Enum.each`函数来遍历字符串，并在每次循环中增加长度的计数器。这种方法的运行时间也是O(n)。

##参考
* [`String.length` 文档](https://hexdocs.pm/elixir/String.html#length/1)
* [`String.trim_leading` 文档](https://hexdocs.pm/elixir/String.html#trim_leading/2)
* [Elixir 字符串教程](https://elixirschool.com/zh-hans/lessons/basics/basics/#%E5%AD%97%E7%AC%A6%E7%AA%97%E6%8E%A5)