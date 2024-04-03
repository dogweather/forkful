---
date: 2024-01-26 01:09:16.374547-07:00
description: "\u5C06\u4EE3\u7801\u5206\u5272\u6210\u51FD\u6570\u610F\u5473\u7740\u5C06\
  \u811A\u672C\u5212\u5206\u4E3A\u66F4\u5C0F\u7684\u3001\u53EF\u91CD\u590D\u4F7F\u7528\
  \u7684\u5757\uFF0C\u8FD9\u4E9B\u5757\u6267\u884C\u7279\u5B9A\u7684\u4EFB\u52A1\u3002\
  \u5B83\u4F7F\u5F97\u4EE3\u7801\u66F4\u52A0\u6E05\u6670\u3001\u6613\u4E8E\u7406\u89E3\
  \uFF0C\u4E14\u66F4\u5BB9\u6613\u8C03\u8BD5\u3002"
lastmod: '2024-03-13T22:44:47.967930-06:00'
model: gpt-4-1106-preview
summary: "\u5C06\u4EE3\u7801\u5206\u5272\u6210\u51FD\u6570\u610F\u5473\u7740\u5C06\
  \u811A\u672C\u5212\u5206\u4E3A\u66F4\u5C0F\u7684\u3001\u53EF\u91CD\u590D\u4F7F\u7528\
  \u7684\u5757\uFF0C\u8FD9\u4E9B\u5757\u6267\u884C\u7279\u5B9A\u7684\u4EFB\u52A1\u3002\
  \u5B83\u4F7F\u5F97\u4EE3\u7801\u66F4\u52A0\u6E05\u6670\u3001\u6613\u4E8E\u7406\u89E3\
  \uFF0C\u4E14\u66F4\u5BB9\u6613\u8C03\u8BD5\u3002."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 什么 & 为什么？
将代码分割成函数意味着将脚本划分为更小的、可重复使用的块，这些块执行特定的任务。它使得代码更加清晰、易于理解，且更容易调试。

## 如何操作：
在Bash中创建一个简单的函数：

```Bash
greet() {
  echo "你好，$1！"
}
```

通过带参数调用函数来使用它：

```Bash
greet "世界"  # 输出：你好，世界！
```

函数可以使用 `return` 返回数值状态码（不用于实际数据返回）：

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # 输出：7
```

请注意 `$?` 捕获上一个命令的返回值，这是 `add` 的数值结果。

## 深入探讨
在Bash中，函数自早期版本以来一直是分隔代码的一种方式。从历史上看，使用函数符合20世纪60年代引入的结构化编程原则，这些原则旨在提高代码质量。

函数的替代方法包括源代码文件或使用别名，但这些不能提供同等级别的模块化和重用。

Bash中一个值得注意的实现细节是，函数是一等公民；它们没有像其他语言中的 `function` 那样的特定声明关键字，尽管在Bash中为了可读性，`function` 是可选的。函数作用域也很有趣——变量默认是全局的，除非声明为局部的，如果不正确管理，可能会导致意外的行为。

## 另请参阅
- Bash手册上关于Shell函数的部分：https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- 高级Bash脚本编程指南：https://tldp.org/LDP/abs/html/functions.html
- 《Pro Bash Programming: Scripting the GNU/Linux Shell》一书，深入探讨函数脚本编写的概念和实践。
