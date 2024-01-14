---
title:                "Gleam: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

对于编程新手来说，经常会遇到一个常见的问题：如何检查一个目录是否存在？这可能是因为他们需要在程序中创建新的目录，或者需要确认某个目录是否存在以便进行后续的操作。无论是什么原因，检查目录是否存在是一个非常重要的编程技巧，它能够帮助我们更好地控制程序的流程。

# 如何操作

要检查一个目录是否存在，我们可以使用Gleam语言提供的`File.exists`函数。这个函数会接受一个字符串作为参数，表示要检查的目录的路径。让我们来看一个具体的例子：

```Gleam
let directory = "/user/desktop/my_folder"

if File.exists(directory) {
  // 目录存在，执行后续操作
  IO.println("目录已存在！")
} else {
  // 目录不存在，提示用户或者执行其他操作
  IO.println("目录不存在，请先创建！")
}
```

如果你运行以上代码，假设目录`/user/desktop/my_folder`存在，输出会是`目录已存在！`，否则会是`目录不存在，请先创建！`。通过这种方式，我们就可以根据不同的结果来执行相应的操作了。

# 深入了解

在Gleam中，我们使用`File.exists`函数来检查一个目录是否存在，但这个函数实际上是调用了操作系统提供的功能来实现的。因此，它的可靠性和效率都是可以得到保证的。同时，它还能够兼容不同的操作系统，比如Linux、Windows等。

此外，对于更复杂的场景，比如需要检查目录下是否存在指定的文件，我们可以使用`File.list`函数来获取目录下的所有文件和文件夹，然后再进行进一步的判断。这些细节可以参考官方文档进行深入研究。

# 参考链接

- [Gleam官方文档](https://gleam.run/documentation/)
- [查看Gleam语言在GitHub上的开源代码](https://github.com/gleam-lang/gleam)
- [访问Gleam语言的论坛社区](https://forum.gleam.run/)