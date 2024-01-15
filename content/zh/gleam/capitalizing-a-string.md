---
title:                "将字符串大写化"
html_title:           "Gleam: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，我们经常需要对字符串进行各种各样的操作。有时候，我们可能需要将字符串中的首字母大写，比如在处理用户输入、生成标题等场景下。因此，学习如何利用Gleam来实现字符串大写操作是一件非常有用的事情。

## 如何操作

在Gleam中，我们可以使用`capitalize`函数来实现字符串大写操作。下面是一个示例代码：

```Gleam
let str = "hello world"
let capitalized = String.capitalize(str)

/* 输出：Hello world */
```

在上述代码中，我们首先定义了一个字符串变量`str`，其值为"hello world"。然后，我们使用`String`模块下的`capitalize`函数来将字符串的首字母大写，并将结果赋值给新的变量`capitalized`。最后，我们打印出`capitalized`的值，即可看到字符串首字母被成功大写的结果。

## 深入了解

除了`capitalize`函数外，Gleam还提供了其他几种方法来实现字符串大写操作。比如，我们可以使用`map`函数来对字符串中的每个字符进行操作。下面是一个示例代码：

```Gleam
let str = "hello world"

let uppercased = String.map(\c -> Char.to_uppercase(c), str)

/* 输出：HELLO WORLD */
```

在上述代码中，我们使用`map`函数将字符串中的每个字符都转换为大写，并将结果赋值给新的变量`uppercased`。最后，我们打印出`uppercased`的值，即可看到字符串被成功地转换为大写格式。

此外，我们还可以使用`fold`函数来实现字符串大写操作。下面是一个示例代码：

```Gleam
let str = "hello world"

let uppercased = String.fold(fun(c, acc) -> Char.to_uppercase(c) ++ acc, "", str)

/* 输出：HELLO WORLD */
```

在上述代码中，我们使用`fold`函数来遍历字符串中的每个字符，并将其转换为大写后拼接到累积变量`acc`中。最后，我们打印出`uppercased`的值，即可看到字符串被成功地转换为大写格式。

## 参考知识

如果你想了解更多关于Gleam中字符串操作的知识，可以参考以下资源：

- Gleam官方文档：[https://gleam.run](https://gleam.run/)
- Gleam String模块文档：[https://gleam.run/modules/string.html](https://gleam.run/modules/string.html)
- Gleam String模块源代码：[https://github.com/gleam-lang/gleam_stdlib/blob/master/src/string.gleam](https://github.com/gleam-lang/gleam_stdlib/blob/master/src/string.gleam)