---
title:    "Ruby: 将字符串转换为小写"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

### 为什么

为什么有时候我们会需要将字符串转换成小写呢？可能是为了方便比较字符串，或者是为了统一格式。无论出于什么目的，掌握如何将字符串转换成小写在编程中是非常有用的技能。

### 如何

首先，我们需要定义一个字符串，例如`my_string = "HELLO WORLD"`

要将字符串转换成小写，我们可以使用`.downcase`方法，如下所示：

```Ruby
my_string.downcase
```

运行以上代码，你会得到输出`"hello world"`。可以看到，所有的大写字母都被转换成了小写字母。

如果要将字符串的第一个字母转换成小写，可以使用`.capitalize`方法。如下所示：

```Ruby
my_string.capitalize
```

运行以上代码，你会得到输出`"Hello world"`。可以看到，只有第一个字母被转换成了小写。

### 深入探讨

在Ruby中，字符串转换成小写的方法非常多，每种方法都有不同的用处。例如，除了使用`.downcase`和`.capitalize`方法，我们还可以使用`.swapcase`方法来将字符串中的大写字母转换成小写，小写字母转换成大写。另外，`.downcase!`和`.capitalize!`方法可以直接修改原字符串，而不是返回一个新的字符串。学习并掌握不同的方法，可以让我们更加灵活地处理字符串。

### 参考资料

- [Ruby Doc: String](https://ruby-doc.org/core-3.0.1/String.html)
- [How to convert a string to lowercase in Ruby](https://www.digitalocean.com/community/tutorials/how-to-convert-a-string-to-lowercase-in-ruby)
- [5 Ways to convert Strings to lowercase in Ruby](https://coderwall.com/p/jekapg/5-ways-to-convert-strings-to-lowercase-in-ruby)

### 参见

- 字符串操作教程
- Ruby基础教程
- [Ruby 入门指南 – 基础部分](https://ruby-china.org/wiki/ruby-beginner-guide#Step_6_23)