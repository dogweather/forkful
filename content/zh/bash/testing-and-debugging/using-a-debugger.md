---
date: 2024-01-26 03:47:34.543449-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Bash\u6CA1\u6709\u50CF\u5176\u4ED6\u4E00\
  \u4E9B\u8BED\u8A00\u90A3\u6837\u5185\u7F6E\u8C03\u8BD5\u5668\uFF0C\u4F46\u4F60\u53EF\
  \u4EE5\u4F7F\u7528\u5185\u7F6E\u547D\u4EE4\u6BD4\u5982`set -x`\u6765\u8FFD\u8E2A\
  \u53D1\u751F\u4E86\u4EC0\u4E48\u3002\u6216\u8005\uFF0C\u4F5C\u4E3A\u5347\u7EA7\u7248\
  \uFF0C\u6709`bashdb`\uFF0C\u4E00\u4E2A\u5408\u9002\u7684\u8C03\u8BD5\u5668\u53EF\
  \u4EE5\u9010\u6B65\u6267\u884C\u4F60\u7684\u4EE3\u7801\u3002\u8FD9\u91CC\u6709\u4E2A\
  \u9884\u89C8\uFF1A."
lastmod: '2024-03-13T22:44:47.966883-06:00'
model: gpt-4-0125-preview
summary: "Bash\u6CA1\u6709\u50CF\u5176\u4ED6\u4E00\u4E9B\u8BED\u8A00\u90A3\u6837\u5185\
  \u7F6E\u8C03\u8BD5\u5668\uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u5185\u7F6E\u547D\
  \u4EE4\u6BD4\u5982`set -x`\u6765\u8FFD\u8E2A\u53D1\u751F\u4E86\u4EC0\u4E48\u3002\
  \u6216\u8005\uFF0C\u4F5C\u4E3A\u5347\u7EA7\u7248\uFF0C\u6709`bashdb`\uFF0C\u4E00\
  \u4E2A\u5408\u9002\u7684\u8C03\u8BD5\u5668\u53EF\u4EE5\u9010\u6B65\u6267\u884C\u4F60\
  \u7684\u4EE3\u7801\u3002\u8FD9\u91CC\u6709\u4E2A\u9884\u89C8\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
