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

## 什么？为什么要检查目录是否存在？
检查是否存在目录是指编程中通过代码确定某个目录是否已经创建或是否存在。程序员在使用特定目录或创建新目录之前需要先确认目录是否存在，以防止错误和干扰。

## 如何进行检查？
Fish Shell提供一个方便的功能来检查目录是否存在，即使用命令“test –d”以及要检查的目录的路径。例如：
```
Fish Shell >> test -d /users/Documents/
 Fish Shell >> echo $status
 0
```
如果目录不存在，将会返回一个错误码，并且输出结果为1。如果目录存在，将会返回结果为0，代表目录存在。

## 深入探讨
在早期的操作系统中，检查目录是否存在是一项基本的功能。不同的操作系统可能有不同的命令来实现此功能，如Windows系统中使用“exist”命令。在Shell编程中，也可以使用其他方法来检查目录是否存在，如使用“ls”命令来查看目录是否存在或者使用通配符来匹配目录名称。而在Fish Shell中，使用“test –d”命令是最常用的方法。

## 参考链接
更多关于Fish Shell的相关信息，请参考以下资源：
- [Fish Shell官方网站](https://fishshell.com/)
- [Fish Shell GitHub仓库](https://github.com/fish-shell/fish-shell)
- [《The Fish Book》](https://fishshell.com/docs/current/index.html)