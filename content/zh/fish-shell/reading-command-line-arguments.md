---
title:                "读取命令行参数"
date:                  2024-01-20T17:55:47.841750-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

category:             "Fish Shell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
命令行参数使程序能够接受额外的用户输入。程序员这样做是为了提升灵活性，让程序根据不同的参数执行不同的任务。

## How to 怎么办？
在Fish中，你使用`$argv`来访问命令行参数。这是个简单的例子：

```Fish Shell
function greet
    for arg in $argv
        echo "你好, $arg"
    end
end
```

运行下面的命令：
```Fish Shell
greet 世界 和平
```

输出将会是：
```
你好, 世界
你好, 和平
```

## Deep Dive 深入探究
Fish Shell从一开始就设计为易用，它处理命令行参数的方式也是体现之一。不同于传统的Bash，Fish不需要特别的语法来读取参数。使用`$argv`就足够了，它是一个列表，包含了所有传递给函数或脚本的参数。

相对地，Bash使用特定的位置参数，如`$1`, `$2`, ..., `$N`来获取相应的输入。

此外，Fish有内建的`argparse`命令来帮助更复杂的参数解析，这让错误处理和默认值设置更加方便。

## See Also 参考链接
- Fish官方文档关于[命令行参数](https://fishshell.com/docs/current/#command-line-arguments)
- Fish官方文档关于[`argparse`](https://fishshell.com/docs/current/cmds/argparse.html)
- [Bash 参数扩展](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html) 供对比参考
