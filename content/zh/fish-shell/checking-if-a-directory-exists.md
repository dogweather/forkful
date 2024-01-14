---
title:                "Fish Shell: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

为什么: 在开始学习使用Fish Shell编程之前，检查文件夹是否存在是一个重要的步骤。这可以帮助我们在编写程序时识别出错误并做出相应的处理。

如何:在Fish Shell中，检测文件夹是否存在很简单。我们可以使用`test -e`命令，并将文件夹路径作为参数传递给它。让我们看一个简单的例子：

```Fish Shell
set directory /home/username/my_directory

if test -e $directory
    echo "Directory exists!"
end
```

输出将会是：“Directory exists!”，这表明我们刚刚检查的文件夹确实存在。

深入了解: 测试文件是否存在的方法可能会有所不同，取决于你的操作系统。在Fish Shell中，你可以使用`test -e`命令来检查文件夹是否存在，但是如果你使用的是Bash Shell，你可能需要使用`[ -d $directory ]`命令。了解这些差异对于正确编写代码至关重要，所以确保你使用的是正确的命令。

另一个需要注意的是，`test`命令也可以用来检查文件是否存在。所以，判断文件是否存在的方法与判断文件夹是否存在的方法是相同的。

参考链接: 
- https://fishshell.com/docs/current/cmds/test.html
- https://www.geeksforgeeks.org/test-command-in-linux-with-examples/ 

## 查看是否存在

参考链接：
- https://fishshell.com/docs/current/cmds/test.html
- https://www.geeksforgeeks.org/test-command-in-linux-with-examples/

现在，你已经知道如何在Fish Shell中检测文件夹是否存在了。这是一个简单但十分实用的命令，可以帮助你更有效地编写你的程序。如果你想要在使用Fish Shell时变得更加熟练，检查文件夹是否存在是一个必不可少的技能。记住不同的Shell可能会有不同的命令，所以确保你使用的是正确的命令来检查文件夹是否存在。现在，你可以尝试使用`test -e`命令来检查不同的文件夹，看看它们是否存在。祝你使用Fish Shell编程愉快！