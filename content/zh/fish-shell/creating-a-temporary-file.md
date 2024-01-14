---
title:                "Fish Shell: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么会创建临时文件

临时文件是在计算机编程中经常用到的，它们可以帮助我们临时存储数据，以便在程序运行期间使用。临时文件也可用于控制资源的访问和管理，因此在Fish Shell编程中创建临时文件是很常见的。

## 如何创建临时文件

在Fish Shell编程中，要创建临时文件，可以使用内置的OMT命令。以下是一个例子：

```Fish Shell
set temp_file (omt)
echo "这是一个临时文件" >> $temp_file
cat $temp_file
```

这个例子中，我们首先使用`omt`命令来创建一个临时文件，然后使用`echo`命令将一些文本写入该文件，最后使用`cat`命令将临时文件的内容打印出来。

## 深入了解创建临时文件

除了使用内置的`omt`命令外，还有其他一些方法来创建临时文件。例如，我们可以使用`mktemp`命令来生成一个带有随机名称的临时文件，如下所示：

```Fish Shell
set temp_file (mktemp)
echo "这是一个临时文件" >> $temp_file
cat $temp_file
```

除此之外，我们还可以使用`> /tmp/temp_file`这样的语法来创建临时文件，该文件会被保存在`/tmp`目录下。不过要注意的是，使用这种方法创建的临时文件并不会自动删除，需要我们手动删除。

# 参考链接

- [Fish Shell官方文档 操作临时文件](https://fishshell.com/docs/current/cmds/omt.html)
- [Linux Shell编程：使用临时文件](https://www.linuxenclave.com/linux-shell-scripting-using-temporary-files/)
- [Linux命令：mktemp](https://linuxize.com/post/linux-mktemp-command/)