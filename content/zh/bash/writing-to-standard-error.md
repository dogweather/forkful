---
title:    "Bash: 写入标准错误"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

在编写Bash脚本时，我们经常会使用标准错误（stderr）来输出错误信息。这样做可以帮助我们调试和发现潜在的问题。因此，学习如何写入标准错误是非常重要的。

## 如何使用

要将信息写入标准错误，我们可以使用特定的符号“2>”，后跟错误信息要输出到的文件。以下是一个简单的例子：
```Bash
echo “这是一个错误信息” 2> error.txt
```

运行上述命令后，错误信息将被写入名为error.txt的文件中。如果我们想要将错误信息输出到屏幕上，可以使用符号“2>&1”，如下所示：
```Bash 
echo “这是一个错误信息” 2>&1
```

现在，错误信息将被输出到终端上，而不是写入文件。

## 深入探讨

写入标准错误还有一些其他的用途。比如，在一些复杂的脚本中，我们可能需要区分不同类型的错误信息并将它们分别写入不同的文件或输出到屏幕上。这就需要我们使用条件语句和重定向来完成。

另外，标准错误还可以与其他Linux命令结合使用。例如，当我们使用管道符号在不同的命令之间传递输出时，如果有错误发生，我们可以使用重定向来将它们捕获并输出到屏幕上。

## 参考链接

了解更多关于标准错误的用法，可以参考以下链接：

- [Linux命令行中的标准错误](https://zhuanlan.zhihu.com/p/140962454)
- [Bash脚本中的重定向](https://blog.csdn.net/yshw1314/article/details/70144997)
- [将标准错误信息输出到屏幕](https://www.cnblogs.com/wbwlu/p/5916931.html)

## 参见 

- [标准输出与标准错误的区别](https://zhuanlan.zhihu.com/p/129707452)
- [如何调试Bash脚本中的错误](https://www.jianshu.com/p/0ad77b8e7eae)
- [Linux命令中的重定向符号](https://linux.cn/article-5374-1.html)