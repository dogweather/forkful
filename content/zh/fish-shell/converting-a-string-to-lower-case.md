---
title:                "Fish Shell: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么
在编程中，有时候我们需要将字符串转换为小写。这可以让我们更容易地比较和操作字符串，尤其是在处理用户输入时。使用Fish Shell来转换字符串到小写可以帮助我们更加高效地完成这个任务。

## 如何做
首先，我们需要定义一个字符串变量。假设我们的字符串是"Hello World"，现在我们想将它转换为小写。以下是Fish Shell中的代码示例：

```Fish Shell
set my_string "Hello World"
echo $my_string | tr [:upper:] [:lower:]
```

运行这段代码，我们将看到输出为"hello world"。让我们来解释一下这段代码。首先，使用`set`命令来定义一个名为`my_string`的变量，并将字符串"Hello World"赋值给它。然后使用`echo`命令来输出我们的字符串，并通过管道将输出传递给`tr`命令。`tr`命令是一个字符串转换工具，可以将一个字符集转换为另一个字符集。在这里，我们使用了`[:upper:]`和`[:lower:]`字符集来将大写字母转换为小写字母。最后，我们可以看到输出已经成功地被转换为小写。

## 深入探讨
实际上，我们可以使用多种方法来将字符串转换为小写。除了通过`tr`命令来转换字符集，我们也可以使用Fish Shell中的内置函数`string tolower`来达到同样的效果。以下是另一种方法的代码示例：

```Fish Shell
set my_string "Hello World"
set my_lower_string (string tolower $my_string)
echo $my_lower_string
```

在这个例子中，我们使用`string tolower`函数来将`my_string`变量中的内容转换为小写，并将结果赋值给一个新的变量`my_lower_string`。最后，我们可以看到输出同样为"hello world"。

最后值得一提的是，不管是使用`tr`命令还是`string tolower`函数，它们都可以接受任意类型的输入，而不仅仅是字符串。这意味着我们也可以将其他数据类型如数字或布尔值转换为小写。

## 参考链接
- Fish Shell官方网站：https://fishshell.com/
- `tr`命令手册：https://fishshell.com/docs/current/cmds/tr.html
- `string tolower`函数手册：https://fishshell.com/docs/current/cmds/string.html#string-lower