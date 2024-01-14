---
title:                "Fish Shell: 创建临时文件"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么

## 为什么要创建临时文件？

临时文件的创建可以帮助我们在编程过程中临时存储数据或者作为程序的中间文件，从而使我们的代码更加整洁、高效。

# 如何

## Fish Shell中创建临时文件的方法

使用Shell中的 `mktemp` 命令可以方便地创建临时文件，示例如下：

```Fish Shell
# 创建一个以 tmp 开头的临时文件
mktemp -t tmp
# 创建一个以 prefix 开头的临时文件
mktemp -p prefix
```

当命令执行成功后，会返回创建的临时文件的完整路径。也可以使用 `echo $TMPDIR` 命令来查看系统设置的临时文件目录。

## 示例输出

```
/tmp/fishXXXXXX
/prefix/fishXXXXXX
```

# 深入了解

创建临时文件的方法可以根据需要进行更多的定制性设置，例如使用 `-d` 参数可以同时创建一个临时文件和对应的临时文件夹。同时，临时文件可以通过 `rm` 命令来清理，在程序结束时可以同时删除临时文件和对应的临时文件夹。

# 参考链接

[Linux Shell-创建临时文件的一些方法](https://blog.csdn.net/csbhahaha/article/details/7627027)

# 另请参阅

[如何在Fish Shell中删除临时文件](https://blog.csdn.net/shadowfaxhmx/article/details/85622459)