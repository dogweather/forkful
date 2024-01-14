---
title:    "Fish Shell: 检查目录是否存在"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 为什么
在编写Fish Shell程序时，经常需要检查特定目录是否存在。这种检查可以确保程序不会因为找不到指定目录而出错。

## 如何
编写Fish Shell程序来检查目录是否存在非常简单。我们可以使用Fish Shell中的测试命令“-d”来检查目录是否存在。下面是一个示例代码，测试目录“/home/user/directory”是否存在：

```Fish Shell
if test -d /home/user/directory
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

运行以上代码，如果目录存在，就会输出“Directory exists”，否则会输出“Directory does not exist”。

## 深入探讨
除了使用测试命令“-d”外，我们还可以使用Fish Shell中的其他测试命令来对目录进行更详细的检查。例如，我们可以使用“-r”命令来检查目录是否可读，使用“-w”命令来检查目录是否可写。

另外，我们也可以使用条件语句来进一步处理我们的程序逻辑。比如，在检查目录存在后，我们可以进一步检查目录中是否存在特定文件，然后执行相应的操作。

总的来说，使用Fish Shell中的测试命令和条件语句，我们可以轻松地在程序中检查目录是否存在，并根据需要进行相应的操作。

## 参考链接
- [Fish Shell官方文档](https://fishshell.com/docs/current/cmds/test.html)
- [使用条件语句在Fish Shell中进行程序控制](https://fishshell.com/docs/current/tutorial.html#tut_conditionals)
- [如何在Fish Shell中检查文件和目录是否存在](https://fishshell.com/docs/current/cmds/test.html)

## 参见
- 查看文件是否存在 - [在Fish Shell中检查文件是否存在](https://example.com/checking-file-exists-fish-shell)