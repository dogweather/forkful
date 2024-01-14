---
title:    "Python: 拼接字符串"
keywords: ["Python"]
---

{{< edit_this_page >}}

# 为什么？

在Python编程中，字符串(string)是不可或缺的基本类型。拼接(concatenate)字符串是将多个字符串连接在一起形成一个新的字符串的过程。拼接字符串在编写程序时经常被使用，它可以帮助我们更灵活的处理文本数据。 

# 如何做？

Python中拼接字符串有多种方式，下面是其中两种常用的方法：

### 方法一：

```
# 定义两个字符串
name = "张三"
age = 25

# 拼接字符串
result = "我叫" + name + "，今年" + str(age) + "岁。"

# 输出结果
print(result)

# 结果将会显示：我叫张三，今年25岁。
```

### 方法二：

```
# 定义两个字符串
name = "张三"
age = 25

# 使用format()函数拼接字符串
result = "我叫{}，今年{}岁。".format(name, age)

# 输出结果
print(result)

# 结果将会显示：我叫张三，今年25岁。
```

在上面的例子中，我们分别定义了两个字符串，然后使用加号和format()函数进行拼接，最终得到我们想要的结果。 

# 深入了解

除了上面提到的两种方式，Python还提供了其他更多的方式来拼接字符串，例如使用join()函数和%操作符，在一些特定的场景下会更加方便和高效。同时，拼接字符串也可以用来处理一些复杂的文本格式化任务，例如生成HTML文件或者构建邮件内容等。

总的来说，拼接字符串是Python编程中必不可少的一部分，熟练掌握拼接字符串的方法能够帮助我们编写出更加灵活和高效的程序。 

# 参考链接

- [Python字符串(string)官方文档(英文)](https://docs.python.org/3/library/string.html)
- [Python字符串(string)教程(中文)](https://www.runoob.com/python3/python3-string.html)
- [Python中的字符串格式化教程(中文)](https://www.runoob.com/python3/python3-string-format.html)

# 参见

- [Markdown语法指南(中文)](https://www.runoob.com/markdown/md-tutorial.html)
- [学习Python编程(中文)](https://www.runoob.com/python3/python3-tutorial.html)