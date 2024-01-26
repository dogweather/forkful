---
title:                "使用调试器"
date:                  2024-01-26T03:47:34.543449-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么和为什么？
在Bash中使用调试器意味着利用工具来测试和找出脚本中的问题，比如捕捉会导致代码崩溃或悄无声息地使其行为异常的错误。程序员之所以这样做，是因为在问题在生产环境中造成混乱之前捕捉到错误显然是更明智的做法。

## 如何操作：
Bash没有像其他一些语言那样内置调试器，但你可以使用内置命令比如`set -x`来追踪发生了什么。或者，作为升级版，有`bashdb`，一个合适的调试器可以逐步执行你的代码。这里有个预览：

```Bash
# 使用 set -x 进行调试
set -x
echo "开始调试"
my_var="你好，调试世界！"
echo $my_var
set +x

# 使用bashdb
# 用你的包管理器安装bashdb，例如，apt、yum、brew。
# 调试一个名为my_script.sh的脚本：
bashdb my_script.sh
```

使用 `set -x` 运行时的输出：
```Bash
+ echo '开始调试'
开始调试
+ my_var='你好，调试世界！'
+ echo '你好，调试世界！'
你好，调试世界！
+ set +x
```

## 深入了解
历史上，调试Bash脚本意味着在代码中到处都是`echo`语句。但随后出现了`set -x`，让我们能够不借助手动打印就窥见运行时执行情况。对于那些渴望更多控制的人来说，受到C/C++中gdb调试器启发的`bashdb`调试器应运而生。

至于替代方案，除了`set`命令（`-x`、`-v`、`-e`）外，其他选项包括将输出重定向到文件以供分析或使用外部工具如ShellCheck进行静态分析。

在实现方面，`set -x`很简单；它是一个原生Bash选项，可以打印出执行的命令及其参数。另一方面，`bashdb`允许逐步执行代码、设置断点和评估表达式 - 这些都是对付更难以捉摸的错误的有力武器。

## 参见
- Bash调试器项目：http://bashdb.sourceforge.net/
- 《高级Bash脚本编程》由Chris Johnson和Jayant Varma撰写。
- ShellCheck进行静态分析：https://www.shellcheck.net/