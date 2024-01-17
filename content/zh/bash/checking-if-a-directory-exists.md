---
title:                "检查文件夹是否存在。"
html_title:           "Bash: 检查文件夹是否存在。"
simple_title:         "检查文件夹是否存在。"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在Bash编程中，我们经常需要检查一个目录是否存在。这是一种用来判断特定目录是否存在的方法，这样我们就可以在程序中进行相应的处理。程序员经常会这么做是为了避免因目录不存在而导致程序出错。

## 如何进行?

```Bash 
if [ -d directory_path ]; then 
  echo "Directory exists!" 
else 
  echo "Directory does not exist." 
fi 
```

输出：

```
Directory exists!
```

在这个例子中，我们使用了一个条件语句来判断目录是否存在。首先，我们使用`-d`标志来检测目录路径是否存在，这个标志代表“目录存在”。如果存在，`if`语句中的第一条命令将会被执行，否则执行`else`语句中的命令。

## 深入了解

### 历史背景

在早期的操作系统中，检查目录是否存在并不是一个常见的需求，因为操作系统通常会自动创建不存在的目录。但是随着Bash的发展，它被认为是一个重要的功能，因为它可以帮助程序员更容易地控制目录的存在与否。

### 其他方法

除了使用`-d`标志外，还可以通过使用`test`命令来检查目录是否存在，其命令格式为`test -d directory_path`。此外，也可以使用`if [ -e directory_path ]; then`来检测目录是否存在，这个方法除了可以检查目录外，还可以检查其他文件类型的存在。

### 实现细节

在Bash中，使用`-d`标志检查目录是否存在，实际上是通过调用`test`命令来完成的。`test`命令会检查指定的文件或目录是否存在，并根据结果返回一个状态码。如果存在，状态码为0，否则为1。

## 参考资料

了解更多关于Bash中检查目录是否存在的知识，请参考以下来源：

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/) 
- [GNU Bash manual](https://www.gnu.org/software/bash/manual/bash.html) 
- [LinuxCommand.org](https://linuxcommand.org/)