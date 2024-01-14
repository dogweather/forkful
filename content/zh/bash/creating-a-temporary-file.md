---
title:    "Bash: 创建临时文件"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么：为什么要创建临时文件？创建临时文件可以在程序运行过程中存储临时数据，以避免数据混乱或重复使用之前的数据。

怎么做：创建临时文件的最基本方法是使用`mktemp`命令。例如，在终端输入以下命令：

```Bash
mktemp -d
```

这将在当前目录中创建一个临时目录，并返回临时目录的路径。您也可以指定文件名或后缀来自定义临时文件名，如下所示：

```Bash
mktemp my_temp_file.XXX
```

这将创建一个名为`my_temp_file.XXX`的文件。 可以使用`-p`参数来指定临时文件的目录，如下所示：

```Bash
mktemp -p /home/user/my_temp_dir my_temp_file.XXX
```

深入探讨：在Linux系统中，临时文件通常存储在`/tmp`或`/var/tmp`目录中。这些文件夹通常在系统启动时自动清理，但也可以手动清理以释放磁盘空间。创建临时文件时，可以使用`TMPDIR`环境变量来指定存储临时文件的目录。临时文件默认权限为`0600`，这意味着只有创建临时文件的用户可以访问它们。如果想让其他用户也能够访问临时文件，可以使用`umask`命令来更改文件的默认权限。

此外，可以使用`trap`命令来捕获程序退出时的错误，以防止临时文件污染系统。在创建临时文件后，使用`trap`命令来删除该临时文件，如下所示：

```Bash
trap "rm -f $TMPFILE" EXIT
```

这样，在程序退出之后，临时文件将自动被删除。

另外，尽量避免在临时文件命名中使用随机数生成器，以防止被恶意创建大量临时文件，从而导致系统崩溃。

相关阅读：如果想要深入了解Bash编程，可以参考以下链接：

- Bash文档：https://www.gnu.org/software/bash/manual/
- BashGuide：https://mywiki.wooledge.org/BashGuide/
- Bash中文站：https://andyhky.github.io/bashTutorial/
- Shell脚本教程（含Bash）：https://wangdoc.com/bash/
- Shell菜鸟教程：https://www.runoob.com/linux/linux-shell.html