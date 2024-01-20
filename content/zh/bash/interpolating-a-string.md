---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么?

字符串插值（string interpolation）是一个编程术语，它会让你插入一个值到字符串里。这样做是为了让我们的代码更清晰易读，同时也可以更方便地拼接字符串。

## 怎么做:

在 Bash 中，有两种常见的方式能进行字符串插值。一种是使用双引号，另一种是使用 printf 函数。这两种方法分别如下：

```Bash
# 使用双引号
VALUE="世界"
echo "你好,${VALUE}"  
# 输出: 你好,世界

# 使用 printf 函数
printf "你好,%s" "$VALUE" 
# 输出: 你好,世界
```

## 深入学习：

字符串插值的概念最早在20世纪60年代的编程语言如ALGOL和COBOL中出现。现在，除了Bash 外，许多其他编程语言（如 Python， JavaScript，Ruby 等）也支持字符串插值。

在 Bash 中，还有其他的字符串操作方法。例如，你也可以使用 `+=` 运算符来拼接字符串：

```Bash
STR="你好,"
STR+="世界"
echo $STR
# 输出: 你好,世界
```

然后，在进行字符串插值时，你应该避免把变量名和字符串文本混淆。为了解决这个问题，你可以在变量名两边加上花括号`{}`。

```Bash
NAME="界"
echo "你好,${NAME}人" 
# 输出: 你好,界人
```

## 参考资源：

1. Bash Programming Guide: http://tldp.org/LDP/abs/html/string-manipulation.html
2. Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/abs-guide.html
3. Bash String Manipulation Examples: https://www.cyberciti.biz/faq/bash-string-manipulation/