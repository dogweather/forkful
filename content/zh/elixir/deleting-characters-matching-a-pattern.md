---
title:                "Elixir: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么

有时候，我们需要在编程中删除一些符合特定模式的字符。这可能是因为我们希望对字符串进行格式化，或者需要清洁数据以进行进一步的处理。无论原因是什么，删除符合特定模式的字符是一个常见的编程任务，特别是在使用Elixir语言的情况下。

## 怎么做

在Elixir中删除符合特定模式的字符非常简单。我们可以使用`String.replace/3`函数来实现。让我们看一个具体的例子：

```Elixir
input = "Hello, world!"
output = String.replace(input, ~r/[a-z]/, "")
IO.puts(output)
```

输出结果为：

```
H , !
```

通过这个例子，我们可以看到`String.replace/3`函数的用法。首先，我们将要处理的字符串存储在`input`变量中。然后，我们使用一个正则表达式作为第二个参数来匹配要删除的字符。在这个例子中，我们的正则表达式是`~r/[a-z]/`，它将会匹配所有的小写字母。最后，我们将空字符串作为第三个参数来替换匹配到的字符。这样就可以将所有小写字母替换为空字符串，从而实现了删除的效果。

## 深入了解

除了上面提到的`String.replace/3`函数，Elixir语言中还有许多其他处理字符串的函数。例如，我们也可以使用`String.delete/2`函数来删除指定的字符，或者使用`String.trim/1`函数来删除字符串两边的空格。另外，Elixir还提供了强大的正则表达式处理能力，可以通过使用`Regex`模块来更灵活地匹配和替换字符。

## 请参阅

- [Elixir中的字符串处理函数官方文档](https://hexdocs.pm/elixir/String.html)
- [Elixir中的正则表达式官方文档](https://hexdocs.pm/elixir/Regex.html)
- [Elixir官方网站](https://elixir-lang.org/)