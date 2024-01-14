---
title:                "Bash: 检查目录是否存在"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

当我们在编写bash脚本时，有时候需要检查一个目录是否已经存在。这是因为当我们需要在特定的目录下创建文件或者执行某些操作时，我们需要确保这个目录是存在的。如果目录不存在，我们的脚本会出现错误，所以我们需要先检查目录是否存在。

# 如何做

在bash中，我们可以使用`-d`选项来检查目录是否存在。下面是一个简单的例子：

```Bash
if [ -d /path/to/directory ]
then
  echo "Directory exists"
fi
```

上面的代码会检查`/path/to/directory`是否存在，如果存在则会打印"目录存在"。我们也可以用`!`来执行相反的操作，即如果目录不存在，则打印相应的消息，如下所示：

```Bash
if [ ! -d /path/to/directory ]
then
  echo "Directory does not exist"
fi
```

我们也可以用变量来代替具体的目录路径，使我们的脚本更加灵活：

```Bash
directory=/path/to/directory
if [ -d $directory ]
then
  echo "Directory exists"
fi
```

# 深入了解

除了`-d`选项外，我们还可以使用其他选项来检查目录是否存在。下面是一些常用的选项：

- `-e`：检查文件或目录是否存在
- `-s`：检查文件是否存在且大小不为0
- `-f`：检查文件是否存在且为常规文件（非目录或链接）
- `-L`：检查目录是否存在且为符号链接
- `-r`：检查目录是否存在且可读
- `-w`：检查目录是否存在且可写
- `-x`：检查目录是否存在且可执行

同时，我们也可以使用组合选项来检查多个条件，如下所示：

```Bash
if [ -d $directory ] && [ -r $directory ]
then
  echo "Directory exists and is readable"
fi
```

# 参考链接

- [Bash - 文档](https://www.gnu.org/software/bash/manual/bash.html)
- [Shell Scripting: Conditions](https://www.linuxjournal.com/content/return-values-bash-functions)
- [Advanced Bash Scripting Guide - Chapter 7](http://tldp.org/LDP/abs/html/testconstructs.html)
- [Mastering the Linux Inline Command](https://opensource.com/article/18/2/running-commands-inside-linux-filesystems)

# 参见

- [如何在Bash中创建文件](https://www.example.com/creating-files-in-bash)
- [如何在Bash中检查文件是否存在](https://www.example.com/checking-if-file-exists-in-bash)