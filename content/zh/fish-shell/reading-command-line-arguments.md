---
title:                "Fish Shell: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么要阅读命令行参数
阅读命令行参数是学习Fish Shell编程的重要一步。在命令行下，我们可以通过读取和解析不同的参数，来实现更高效和灵活的脚本编写。

# Fish Shell 如何读取命令行参数
在Fish Shell中，读取命令行参数非常简单。我们可以通过使用 `$argv` 表示命令行参数。例如，如果我们运行以下命令：
```
fish script.fish hello world
```
那么，`$argv[1]` 将会是 `hello`，`$argv[2]` 将会是 `world`。如果我们需要读取所有的参数，可以使用 `$argv[1..-1]` 来表示所有的命令行参数。让我们来看一个示例代码：
```Fish Shell
#!/usr/bin/fish

echo "我收到了这些命令行参数：$argv[1..-1]"
```
输出将会是：
```
我收到了这些命令行参数：hello world
```

# 深入探讨命令行参数的读取
除了简单地读取命令行参数之外，我们还可以使用一些特殊的标记来控制命令行参数的读取。例如，在参数前面加上两个减号 `--`，将会使它变成一个命令行选项。我们可以通过使用 `-s` 来表示 `--silent`。让我们来看一个完整的示例代码：
```Fish Shell
#!/usr/bin/fish

# 定义一些选项的默认值
set silent false
set name "Anonymous"

# 检查命令行参数并设置选项的值
for arg in $argv
    switch $arg
        case "-s" or "--silent"
            set silent true
        case "-n" or "--name"
            set name $argv[(math $argv | grep -n $arg | cut -d ":" -f 1) + 1]
end

# 执行相应的操作
if $silent
    echo "欢迎，$name。我是一个安静的脚本。"
else
    echo "欢迎，$name。我是一个有声音的脚本。"
end
```
如果我们运行以下命令：
```
fish script.fish --silent --name Lucy
```
输出将会是：
```
欢迎，Lucy。我是一个安静的脚本。
```

# 参考链接
- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub仓库](https://github.com/fish-shell/fish-shell)
- [Fish Shell 文档翻译项目](https://github.com/oh-my-fish/docs)
- [Linux命令行基础教程](https://www.linuxcommand.org/lc3_learning_the_shell.php)