---
title:                "计算字符串的长度"
html_title:           "Bash: 计算字符串的长度"
simple_title:         "计算字符串的长度"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编写Bash程序时，有时需要获取字符串的长度。这可以帮助我们更好地处理文本数据，从而使我们的程序更加强大和灵活。

## 如何

```Bash
# 创建一个变量来存储字符串
my_string="这是一个字符串。"

# 使用内置的`expr`命令来计算字符串的长度
string_length=$(expr length "$my_string")

# 打印结果
echo $string_length

# 输出：15
```

```Bash
# 如果需要在同一行中执行此操作，可以使用命令子换行符`\n`：
# 创建一个变量来存储字符串
my_string="这是一个字符串。"

# 创建一个变量来存储字符串长度
string_length=$(expr length "$my_string")

# 在终端中打印结果
echo -e "字符串的长度是: $string_length"

# 输出：字符串的长度是: 15
```

## 深入了解

字符串的长度是指字符串中包含的字符的数量。在Bash中，可以使用内置的`expr`命令来计算字符串的长度。这个命令有多种用法，但是当我们想获得字符串的长度时，可以使用`length`选项。值得注意的是，在使用`expr`命令时，需要在字符串和运算符之间添加一个空格，否则会报错。

## 参考链接

- [Bash的官方文档（英文）](https://www.gnu.org/software/bash/manual/bash.html)
- [操作字符串的方式（英文）](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Unix Toolbox（英文）](http://cb.vu/unixtoolbox.zh_2.html)
- [Mastering Bash（英文）](https://www.thomas-krenn.com/en/wiki/Mastering_Bash)

## 参见