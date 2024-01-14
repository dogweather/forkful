---
title:    "Bash: 检查目录是否存在"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 为什么

对于那些想要在Bash编程中了解如何检查目录是否存在的读者来说，这是一个非常有用的技巧。通过检查目录是否存在，程序可以避免出现无法找到所需文件或目录的错误，并在必要时采取适当的措施。

## 如何操作

要检查一个目录是否存在，可以使用`[ -d ]`命令。以下是一个简单的例子：

```Bash
if [ -d "/home/user/my_directory" ]; then
  echo "my_directory存在"
else
  echo "my_directory不存在"
fi
```

输出将取决于`/home/user/my_directory`是否存在。如果目录存在，输出将是“my_directory存在”，否则将是“my_directory不存在”。

## 深入探讨

除了使用`[ -d ]`命令，还可以使用`test -d`命令来检查目录是否存在。这两个命令的作用是相同的，它们都将在目录存在时返回true(0)，在目录不存在时返回false(1)。如果读者想要了解更多关于这两个命令的区别，请查看[这篇文章](https://www.shell-tips.com/bash/test-vs/)。

## 参考链接

- [Bash教程(中文)](https://wangdoc.com/bash/)
- [Linux Shell脚本教程(中文)](https://www.runoob.com/linux/linux-shell.html)
- [Bash常用命令大全(中文)](https://www.cnblogs.com/sysu-blackbear/p/4532044.html)

## 参见

- [如何在Bash中创建目录的指南(中文)](https://www.dataguru.cn/article-16340-1.html)
- [如何在Bash中检测文件是否存在的方法(中文)](https://www.shiyanlou.com/courses/1/labs/1009/document)