---
date: 2024-01-26 01:09:16.374547-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Bash\u4E2D\uFF0C\u51FD\u6570\u81EA\
  \u65E9\u671F\u7248\u672C\u4EE5\u6765\u4E00\u76F4\u662F\u5206\u9694\u4EE3\u7801\u7684\
  \u4E00\u79CD\u65B9\u5F0F\u3002\u4ECE\u5386\u53F2\u4E0A\u770B\uFF0C\u4F7F\u7528\u51FD\
  \u6570\u7B26\u540820\u4E16\u7EAA60\u5E74\u4EE3\u5F15\u5165\u7684\u7ED3\u6784\u5316\
  \u7F16\u7A0B\u539F\u5219\uFF0C\u8FD9\u4E9B\u539F\u5219\u65E8\u5728\u63D0\u9AD8\u4EE3\
  \u7801\u8D28\u91CF\u3002 \u51FD\u6570\u7684\u66FF\u4EE3\u65B9\u6CD5\u5305\u62EC\u6E90\
  \u4EE3\u7801\u6587\u4EF6\u6216\u4F7F\u7528\u522B\u540D\uFF0C\u4F46\u8FD9\u4E9B\u4E0D\
  \u80FD\u63D0\u4F9B\u540C\u7B49\u7EA7\u522B\u7684\u6A21\u5757\u5316\u548C\u91CD\u7528\
  \u3002\u2026"
lastmod: '2024-04-05T22:51:01.177162-06:00'
model: gpt-4-1106-preview
summary: "\u51FD\u6570\u7684\u66FF\u4EE3\u65B9\u6CD5\u5305\u62EC\u6E90\u4EE3\u7801\
  \u6587\u4EF6\u6216\u4F7F\u7528\u522B\u540D\uFF0C\u4F46\u8FD9\u4E9B\u4E0D\u80FD\u63D0\
  \u4F9B\u540C\u7B49\u7EA7\u522B\u7684\u6A21\u5757\u5316\u548C\u91CD\u7528\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

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
