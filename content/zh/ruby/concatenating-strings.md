---
title:                "连接字符串"
html_title:           "Ruby: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么：当您需要将多个字符串拼接在一起时，学习如何使用Ruby中的字符串拼接功能可以帮助您更有效地处理多个数据。

如何使用：使用`+`运算符可以将两个字符串连接在一起，并且可以使用`<<`运算符来实现原地修改第一个字符串。例如：

```Ruby
puts "Hello" + " World"

name = "Ruby"
name << " Programming"
puts name
```

输出：

`Hello World`

`Ruby Programming`

深入探讨：可以使用`concat`方法来拼接多个字符串，也可以通过使用`join`方法和数组来将多个字符串拼接在一起。另外，可以使用字符串插值来方便地拼接变量和字符串。例如：

```Ruby
str_1 = "This is"
str_2 = "a sentence"
puts str_1.concat(" ", str_2)

array = ["Ruby", "is", "fun"]
puts array.join(" ")
```

输出：

`This is a sentence`

`Ruby is fun`

另外，字符串拼接也可以帮助我们将不同类型的数据转换为字符串并进行拼接。例如：

```Ruby
num = 1
puts "The number is " + num.to_s
```

输出：

`The number is 1`

可以通过这种方式来简单地将数字等其他类型的数据与字符串拼接在一起。

## 参考链接

- [Ruby官方文档：String](https://ruby-doc.org/core-2.7.2/String.html)
- [Ruby字符串拼接方法详解](https://www.jianshu.com/p/647643bd5ae4)
- [Ruby字符串简单理解](https://www.cnblogs.com/hyl8218/p/9696111.html)

## 参见

- [Ruby字符串常用方法简介](https://zhuanlan.zhihu.com/p/63630405)
- [Ruby中最常用的字符串操作方法](https://blog.csdn.net/u011400125/article/details/82253127)
- [Ruby字符串插值](https://ruby-china.org/topics/27443)