---
title:                "连接字符串"
html_title:           "Bash: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么与为什么？

字符串拼接指的是把两个或多个字符串连接在一起形成单个字符串。程序员之所以需要进行字符串拼接，主要是为了组合和调整字符串信息以满足特定的编程需求。

## 如何做：

使用Bash拼接字符串十分简单。以下是一些示例代码和其输出:

```Bash
# 定义变量
str1="你好，"
str2="世界!"
# 字符串拼接
greeting=$str1$str2
# 打印结果
echo $greeting
```

输出:
```Bash
你好，世界!
```

## 深入探讨

历史上，Bash并没有专门的字符串拼接运算符，而是简单地把字符串或变量放在一起，Bash就会执行拼接操作。至于替代方案，可以通过printf函数或+=运算符实现字符串拼接，但最直接的方式仍然是直接使用变量进行拼接。在字符串拼接的过程中，Bash实际上是创建了一个新的字符串，然后在其上添加了指定的字符串。

## 延伸阅读

对于Bash编程中字符串拼接的更多信息，可以参考以下文档或教程：
1. Bash手册：http://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
2. Advanced Bash-Scripting Guide：http://tldp.org/LDP/abs/html/
3. Bash编程教程：https://ryanstutorials.net/bash-scripting-tutorial/bash-strings.php