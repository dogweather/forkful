---
title:                "Bash: 创建临时文件"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

不论是作为初学者或者有经验的程序员，都会遇到创建临时文件的情况。临时文件在编程中扮演着重要的角色，它们可以充当日志文件、缓存文件或者是暂存数据的存储空间。因此，掌握创建临时文件的方法是非常有用的。

## 如何

在Bash编程中，可以通过使用 `mktemp` 命令来创建临时文件。其基本语法如下：

```Bash
TEMPFILE=$(mktemp)
```

这条命令将会在默认的临时文件目录中创建一个唯一的文件并将其赋值给 `TEMPFILE` 变量。也可以通过指定文件名前缀来创建有意义的文件名，例如：

```Bash
LOGFILE=$(mktemp log.XXXXX)
```

其中， `XXXXX` 代表随机的字符串。这样可以让我们更容易地识别出不同的临时文件。

除了使用 `mktemp` 命令，我们也可以使用 `touch` 命令来创建临时文件：

```Bash
touch /tmp/tempfile
```

但是需要注意的是，这种方法创建的文件可能不够安全，因为其他用户也有可能在同一时间创建相同名字的文件。

创建临时文件之后，可以在其中写入数据，例如：

```Bash
TEMPFILE=$(mktemp)
echo "Hello World!" >> $TEMPFILE
```

## 深入了解

当我们在创建临时文件的时候，有时候需要指定文件的权限。在使用 `mktemp` 命令时，可以使用 `-m` 参数来指定文件的权限，例如：

```Bash
TEMPFILE=$(mktemp -m 777)
```

这样就会在创建文件的同时，将其权限设置为 `777`，即所有用户都有读写执行的权限。

另外，可以使用 `-d` 参数来创建临时文件夹：

```Bash
TEMPDIR=$(mktemp -d)
```

这样就可以在需要的时候创建临时文件夹来存储临时文件。

## 参考链接

- [Bash脚本教程](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Shell脚本编程基础](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [Shell脚本基础语法](https://www.tldp.org/LDP/abs/html/basic-syntax.htm)

## 延伸阅读

- [如何在Linux中管理临时文件](https://www.howtogeek.com/213899/how-to-manage-temporary-files-in-linux/)
- [Shell脚本中的命令替换](https://www.cyberciti.biz/faq/unix-linux-replace-string-words-in-many-files/)
- [Linux命令参考手册](https://www.linuxcommand.org/)