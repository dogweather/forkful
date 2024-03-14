---
date: 2024-01-26 03:47:34.543449-07:00
description: "\u5728Bash\u4E2D\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u5229\
  \u7528\u5DE5\u5177\u6765\u6D4B\u8BD5\u548C\u627E\u51FA\u811A\u672C\u4E2D\u7684\u95EE\
  \u9898\uFF0C\u6BD4\u5982\u6355\u6349\u4F1A\u5BFC\u81F4\u4EE3\u7801\u5D29\u6E83\u6216\
  \u6084\u65E0\u58F0\u606F\u5730\u4F7F\u5176\u884C\u4E3A\u5F02\u5E38\u7684\u9519\u8BEF\
  \u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u56E0\u4E3A\
  \u5728\u95EE\u9898\u5728\u751F\u4EA7\u73AF\u5883\u4E2D\u9020\u6210\u6DF7\u4E71\u4E4B\
  \u524D\u6355\u6349\u5230\u9519\u8BEF\u663E\u7136\u662F\u66F4\u660E\u667A\u7684\u505A\
  \u6CD5\u3002"
lastmod: '2024-03-13T22:44:47.966883-06:00'
model: gpt-4-0125-preview
summary: "\u5728Bash\u4E2D\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u5229\u7528\
  \u5DE5\u5177\u6765\u6D4B\u8BD5\u548C\u627E\u51FA\u811A\u672C\u4E2D\u7684\u95EE\u9898\
  \uFF0C\u6BD4\u5982\u6355\u6349\u4F1A\u5BFC\u81F4\u4EE3\u7801\u5D29\u6E83\u6216\u6084\
  \u65E0\u58F0\u606F\u5730\u4F7F\u5176\u884C\u4E3A\u5F02\u5E38\u7684\u9519\u8BEF\u3002\
  \u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u56E0\u4E3A\u5728\
  \u95EE\u9898\u5728\u751F\u4EA7\u73AF\u5883\u4E2D\u9020\u6210\u6DF7\u4E71\u4E4B\u524D\
  \u6355\u6349\u5230\u9519\u8BEF\u663E\u7136\u662F\u66F4\u660E\u667A\u7684\u505A\u6CD5\
  \u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
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
