---
title:    "Bash: 检查目录是否存在"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

为什么会使用Bash编程:

Bash编程是一种强大的工具，可以帮助你自动化许多重复的任务。通过编写Bash脚本，你可以检查文件和文件夹的存在性，以确保你的程序能够顺利运行。这样可以节省大量的时间和精力，并且有效地确保程序的可靠性和稳定性。

如何进行检查文件夹是否存在：

首先，你需要使用`if`语句来判断文件夹是否存在。在Bash中，可以使用`-d`选项来检查一个路径是否为文件夹。以下是一个示例代码：

```
if [ -d "example_folder" ]; then
  echo "文件夹存在"
else
  echo "文件夹不存在"
fi
```

在上面的代码中，我们使用了`if`语句和条件表达式`-d`来判断`example_folder`是否为文件夹。如果检测到文件夹存在，就会打印出"文件夹存在"，否则打印出"文件夹不存在"。

深入了解检查文件夹是否存在：

除了使用`-d`选项来检查文件夹是否存在，Bash还提供了其他几种选项来检测文件和文件夹的存在性。这些选项包括`-e`用于检查文件或文件夹是否存在，`-f`用于检查文件是否存在，`-s`用于检查文件是否为空，以及`-x`用于检查文件是否可执行。你可以根据自己的需求来选择合适的选项进行判断。

另外，如果你想要在程序中创建文件夹，你可以使用`mkdir`命令，如果需要同时创建多个文件夹，可以使用`mkdir -p`命令。

同样重要的是，如果你需要删除文件夹，可以使用`rmdir`命令，但是请注意，`rmdir`只能删除空文件夹，如果想要删除非空文件夹，可以使用`rm -r`命令。

请参阅:

- [Bash教程](https://www.runoob.com/linux/linux-shell.html)
- [Bash文档](https://www.gnu.org/software/bash/manual/bash.html)
- [Linux命令大全](https://man.linuxde.net/)