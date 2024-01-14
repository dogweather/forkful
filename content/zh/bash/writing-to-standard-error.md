---
title:    "Bash: 标准错误的写作"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 为什么

为什么写入标准错误很重要？在编写Bash脚本时，我们可能会遇到各种错误。通过将错误信息写入标准错误，我们可以更方便地调试和定位问题。

## 如何

要将错误信息写入标准错误，我们可以使用`>&2`来重定向输出。以下是一个简单的例子：

```Bash
#!/bin/bash
echo "这是标准输出"
echo "这是标准错误" >&2
```

运行该脚本后，我们可以看到标准输出和标准错误分别显示在屏幕上：

```
这是标准输出
这是标准错误
```

## 深入了解

在深入了解如何写入标准错误之前，我们需要了解标准输出和标准错误的概念。在Bash中，标准输出（stdout）和标准错误（stderr）是两种不同的输出流。标准输出通常用于输出正常的程序结果，而标准错误用于输出错误信息和警告。通过将错误信息写入标准错误而不是标准输出，我们可以更方便地区分两者，并且在调试时可以更加有效地定位问题。

另外，我们也可以通过重定向符号`2>`来将标准错误写入到指定的文件或者/dev/null中。这样做可以帮助我们将错误信息保存下来，以便后续分析和处理。

## 参考资料

- [Unix/Linux标准输出与标准错误的重定向](https://www.cnblogs.com/qdhxhz/p/14366130.html)
- [Bash shell中的标准输出和标准错误](https://linuxize.com/post/bash-output-redirection/)
- [Bash错误处理](https://bash.cyberciti.biz/guide/Error_handling)

## 参见

- [Bash详细教程](https://www.ruanyifeng.com/blog/2020/04/bash-tutorial.html)
- [Linux命令行快速入门](https://www.linux.com/learn/getting-started-bash-command-line-linux/)