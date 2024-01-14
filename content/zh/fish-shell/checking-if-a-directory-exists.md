---
title:                "Fish Shell: 检查目录是否存在"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

在进行Fish Shell编程时，有时会遇到需要检查目录是否存在的情况。这样做可以确保程序在操作目录时不会出现错误，提高程序的稳定性和可靠性。

# 如何操作

```Fish Shell
if test -d /path/to/directory
  echo "目录存在"
else
  echo "目录不存在"
end
```

以上代码会检查指定路径下的目录是否存在，如果存在则会输出“目录存在”，否则输出“目录不存在”。

# 深入探讨

在Fish Shell中，可以通过`test`命令来检测文件或目录是否存在。`test`命令的语法为`test -d <文件或目录路径>`，其中`-d`表示要检查的是一个目录。如果文件或目录存在，则返回`true`，否则返回`false`。

除了`test`命令，还可以使用`if`语句的`-d`选项来检查文件或目录是否存在。例如：

```Fish Shell
if -d /path/to/directory
  echo "目录存在"
else
  echo "目录不存在"
end
```

使用`-d`选项，可以让代码更加简洁和易读。

# 参考资料

- [Fish Shell官方文档-测试自定义条件](https://fishshell.com/docs/current/commands.html#test)
- [Fish Shell官方文档-if语句](https://fishshell.com/docs/current/index.html#if)
- [Linux命令大全](https://www.linuxcool.com/)

# 参见

- [Fish Shell语法基础](https://www.example.com/fish-shell-basic-syntax)
- [Fish Shell环境变量设置](https://www.example.com/fish-shell-environment-variables)