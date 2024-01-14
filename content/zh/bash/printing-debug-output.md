---
title:    "Bash: “打印调试输出”"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么

当我们编写脚本或程序时，经常会遇到一些错误和bug。在解决这些问题的过程中，打印调试输出（debug output）是一个非常有用的工具。通过打印输出，我们可以了解程序的执行情况，找出问题所在并进行调试。因此，打印调试输出是提高脚本和程序可靠性的重要步骤。

## 如何打印调试输出

在Bash编程中，我们可以使用`echo`命令来打印调试输出。下面是一个简单的例子：

```Bash
#!/bin/bash
# 定义一个变量
my_var="Hello World"
# 打印调试输出
echo "My variable is: $my_var"
```

这段代码会打印出`My variable is: Hello World`，这样我们就能够确认变量的值是否正确。除了使用变量，我们也可以打印其他信息，比如当前的目录路径、命令执行结果等等。

## 深入了解打印调试输出

除了使用`echo`命令，我们也可以使用`printf`命令来打印调试输出。`printf`命令有更多的格式控制选项，可以使调试输出更加具有可读性。另外，我们还可以通过使用`set -x`命令来启用Bash的追踪模式。在追踪模式下，Bash会将每条命令和其参数都打印出来，方便我们进行调试。

## 参考资料

- [Bash脚本编程入门教程](https://linux.cn/article-5648-1.html)
- [Bash调试技术详解](http://www.yunweipai.com/archives/20421.html)

## 参考链接

- [Echo Command](https://www.geeksforgeeks.org/echo-command-in-linux-with-examples/)
- [Debugging Bash Scripts](https://linuxize.com/post/debugging-bash-scripts/)