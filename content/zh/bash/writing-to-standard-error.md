---
title:                "写入标准错误"
html_title:           "Bash: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

###

什么&为什么？

写入标准错误是指将程序中产生的错误信息输出到终端窗口中。程序员这样做是为了让用户能够看到程序的错误，从而帮助调试和改进程序。

如何：

有几种方法可以将错误信息写入标准错误。下面是一个例子：

```Bash
if [ $x -lt 0 ]; then
  echo "x不能为负数" >&2
  exit 1
fi
```

输出：

x不能为负数

深入探讨：

写入标准错误的概念并不是新鲜的。早期的Unix系统已经使用这种方法来显示程序的错误信息。除了将错误信息写入标准错误，程序员也可以选择将它们写入日志文件。

另一种方法是使用标准错误重定向符号"2>"来指定将错误信息写入哪个文件。这种方法需要更复杂的代码来处理错误信息，因为程序不直接将它们输出到终端窗口。

建议阅读：

如果想了解更多关于标准错误的内容，可以参考以下资源：

2. [GNU Bash文档](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)

请注意，这些资源是英文的，需要有一定的英文阅读能力。同时，也可以参考其他中文的博客和教程来学习标准错误的应用。