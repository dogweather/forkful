---
date: 2024-01-26 04:14:13.545339-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Fish\u4E2D\uFF0C\u4EA4\u4E92\u5F0F\
  Shell\u5728\u542F\u52A8\u65F6\u9ED8\u8BA4\u4E3A\u9ED8\u8BA4\u6A21\u5F0F\u3002\u4EE5\
  \u4E0B\u662F\u5B9E\u9645\u64CD\u4F5C\u7684\u6837\u5B50\uFF1A."
lastmod: '2024-04-05T22:38:47.405538-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Fish\u4E2D\uFF0C\u4EA4\u4E92\u5F0F\
  Shell\u5728\u542F\u52A8\u65F6\u9ED8\u8BA4\u4E3A\u9ED8\u8BA4\u6A21\u5F0F\u3002\u4EE5\
  \u4E0B\u662F\u5B9E\u9645\u64CD\u4F5C\u7684\u6837\u5B50\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 如何操作：
在Fish中，交互式Shell在启动时默认为默认模式。以下是实际操作的样子：

```Fish Shell
> set color blue
> echo "The sky is $color"
天空是蓝色
```

您还可以运行内置函数并尝试命令替换：

```Fish Shell
> function cheer
      echo "Go Fish $argv!"
  end
> cheer Coders
Go Fish Coders!
```

不仅仅是定义函数，您也可以即时执行代码片段并立即看到输出：

```Fish Shell
> math "40 / 2"
20
```

## 深入探讨
REPL的概念可以追溯到1960年代的Lisp编程语言。这种形式的交互式编程为像Python的`ipython`和Ruby的`irb`这样的环境设定了标准。Fish继续这一趋势，专注于用户友好性和交互式使用。

Fish与其他像Bash这样的Shell不同，在设计之初就考虑到了交互性。它提供了语法高亮、自动建议和标签补全，这使得它在REPL风格的工作流中非常强大。更好的是，您的命令会被记住并且可以搜索，使得重复测试变得轻而易举。

Fish的REPL的替代品可能是`bash`或`zsh`，当与诸如`bash-completion`或`oh-my-zsh`之类的扩展配对时，但Fish往往提供更丰富的开箱即用体验。

## 另请参阅：
- Fish文档：https://fishshell.com/docs/current/index.html
- Fish与其他Shell的有趣比较：https://www.slant.co/versus/2209/3686/~fish_vs_bash
- 深入了解REPLs：https://en.wikipedia.org/wiki/Read–eval–print_loop
- Lisp中的交互式编程，一个历史回顾：http://www.paulgraham.com/ilisp.html
