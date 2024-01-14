---
title:    "Ruby: 字符串连接"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么

为什么要学习Ruby编程？一种常见的技术是在字符串中连接其他字符串，这在许多不同的编程任务中都非常有用。在本文中，我们将深入探讨如何使用Ruby连接字符串以及为什么它是一个强大的编程技巧。

# 如何使用

连接字符串是指将多个字符串合并成一个字符串。这在编程中经常用于创建用户界面文本、生成数据报告以及搜索字符串匹配。让我们看一个简单的例子来了解如何在Ruby中连接字符串：

```Ruby
  # 创建两个字符串变量
  first_name = "张"
  last_name = "三"

  # 使用字符串插值语法连接变量
  full_name = "#{first_name} #{last_name}"
  puts full_name
```

输出将会是：张三

在这个例子中，我们使用了Ruby的字符串插值语法（#{}）来连接两个字符串变量，并通过puts方法将结果打印出来。你也可以使用简单的加号（+）来连接字符串：

```Ruby
  first_name = "张"
  last_name = "三"

  full_name = first_name + " " + last_name
  puts full_name
```

输出同样是：张三

另外，你也可以使用Ruby的concat方法来连接字符串，如下所示：

```Ruby
  first_name = "张"
  last_name = "三"

  full_name = first_name.concat(" ", last_name)
  puts full_name
```

输出同样是：张三

# 深入探讨

除了连接简单的字符串，你也可以在Ruby中连接其他数据类型，如数字和布尔值。当你连接一个数字和一个字符串时，Ruby会将数字转换为字符串：

```Ruby
  # 创建一个数字变量
  age = 25
  puts "我今年" + age + "岁。"
```

输出将会是：我今年25岁。

当你连接布尔值时，Ruby会将其转换为字符串“true”或“false”：

```Ruby
  # 创建一个布尔值变量
  is_ruby_fun = true
  puts "学习Ruby是" + is_ruby_fun + "的。"
```

输出将会是：学习Ruby是true的。

另外，你还可以使用Ruby的join方法来连接数组中的多个元素：

```Ruby
  # 创建一个包含几个动物的数组
  animals = ["狗", "猫", "熊"]

  # 使用join方法连接数组中的元素
  puts "我最喜欢的动物是" + animals.join("，") + "。"
```

输出将会是：我最喜欢的动物是狗，猫，熊。

除了以上提到的方法，Ruby还有许多其他的字符串连接技巧，如使用<<运算符、使用concat方法连接多个字符串等等。通过不断学习和练习，你将可以灵活地使用这些技巧来解决各种编程问题。

＃＃见下文

- [Ruby字符串连接方法](https://www.rubyguides.com/2011/07/ruby-string-concat-methods/)
- [Ruby字符串插值语法](https://www.rubyguides.com/2019/02/ruby-string-interpolation/)
- [Ruby字符串文档](https://ruby-doc.org/core-2.5.1/String.html)