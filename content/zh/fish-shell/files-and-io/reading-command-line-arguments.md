---
date: 2024-01-20 17:55:47.841750-07:00
description: "How to \u600E\u4E48\u529E\uFF1F \u5728Fish\u4E2D\uFF0C\u4F60\u4F7F\u7528\
  `$argv`\u6765\u8BBF\u95EE\u547D\u4EE4\u884C\u53C2\u6570\u3002\u8FD9\u662F\u4E2A\u7B80\
  \u5355\u7684\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.422351-06:00'
model: gpt-4-1106-preview
summary: "How to \u600E\u4E48\u529E\uFF1F \u5728Fish\u4E2D\uFF0C\u4F60\u4F7F\u7528\
  `$argv`\u6765\u8BBF\u95EE\u547D\u4EE4\u884C\u53C2\u6570\u3002\u8FD9\u662F\u4E2A\u7B80\
  \u5355\u7684\u4F8B\u5B50\uFF1A."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
