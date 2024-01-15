---
title:                "检查目录是否存在"
html_title:           "Fish Shell: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编写代码或脚本时，经常会需要检查某个目录是否存在，以确保程序的正常运行，避免出现错误。使用Fish Shell提供的内置命令，可以轻松地检查目录是否存在，节省时间和精力。

## 如何使用

```Fish Shell
# 检查目录是否存在
if test -d "目录路径"
	echo "目录存在"
else
	echo "目录不存在"
end
```

这个例子中，我们使用了内置命令```test -d```来检查指定目录是否存在。如果存在，则会输出“目录存在”，否则会输出“目录不存在”。

## 深入了解

Fish Shell的判断命令```test```包含许多选项，其中包括```-d```用于检查目录是否存在。你也可以使用```-e```选项来检查文件或目录是否存在。除此之外，```test```命令还有其他选项可用于检查文件的权限、大小等。

## 参考链接

- [Fish Shell官方文档](https://fishshell.com/docs/current/commands.html#test)
- [Fish Shell GitHub仓库](https://github.com/fish-shell/fish-shell)
- [Linux命令教程](https://www.runoob.com/linux/linux-command-manual.html)