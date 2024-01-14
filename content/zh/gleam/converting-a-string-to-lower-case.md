---
title:    "Gleam: 将字符串转换为小写"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 为什么要将字符串转换为小写？

在编程中，有时候我们需要将一个字符串转换为小写。这可能是因为我们需要比较字符串时忽略大小写，或者为了美观而将字符串显示为小写。无论原因是什么，转换字符串到小写是一个常见的任务，因此我们应该掌握如何做到这一点。

## 如何做到？

```Gleam
string.to_lower("Hello World!")
```

输出结果: "hello world!"

```Gleam
string.to_lower("123ABC")
```

输出结果: "123abc"

在Gleam中，我们可以使用内置的 `string` 模块来方便地将字符串转换为小写。我们只需要调用 `string.to_lower`，并将需要转换的字符串作为参数传递即可。

## 深入了解

值得注意的是，字符串转换为小写的结果取决于当前的本地化设置。例如，对于中文来说，大写的 "A" 可能被转换为小写的 "ā"，而不是常见的 "a"。因此，在处理不同语言的字符串时，我们需要注意这一点。

此外，在一些编程语言中，字符串的不可变性（immutable）可能会导致转换字符串到小写时，生成一个新的字符串对象。但在Gleam中，我们可以使用 `string.to_lower_mut` 来原地修改字符串，从而提高性能。

## 参考链接

[The Gleam Standard Library - string module](https://gleam.run/stdlib/string.html#string.to_lower)
[Gleam语言官方文档](https://gleam.run/)