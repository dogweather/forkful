---
title:                "Python: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么：

字符串连接是编程中常见的操作，它可以将多个字符串结合在一起，使得程序可以更灵活地处理文本数据。

## 为什么要使用字符串连接？

字符串连接通常被用来构建复杂的文本输出，比如将用户的输入和固定文本结合起来生成个性化的输出。它也可以用于生成动态的文件名或者URL链接。不管是什么目的，字符串连接都为程序提供了必要的灵活性和可定制性。

## 如何实现字符串连接？

在Python中，使用加号`+`可以将两个字符串连接起来，例如：

```Python
name = "王小明"
greeting = "你好，"
message = greeting + name
print(message)
```

输出结果为：

```Python
你好，王小明
```

如果需要连接多个字符串，可以使用`+`运算符多次连接，或者使用`join()`方法，例如：

```Python
fruits = ["苹果", "香蕉", "橘子"]
output = "我喜欢吃" + fruits[0] + "、" + fruits[1] + "和" + fruits[2]
print(output)

output = "、".join(fruits)
print("我喜欢吃" + output)
```

输出结果为：

```Python
我喜欢吃苹果、香蕉和橘子
我喜欢吃苹果、香蕉、橘子
```

## 深入理解字符串连接

在Python中，字符串的连接操作其实是创建了一个新的字符串对象。每次使用`+`运算符连接字符串时，都会创建一个新的字符串对象，因此在循环中频繁使用字符串连接可能会产生大量的中间对象，影响程序的效率。为了避免这种情况，可以使用`join()`方法将字符串存储在一个列表中，再使用`join()`方法连接所有字符串。这样可以大大提高程序的效率。

此外，字符串连接时需要注意数据类型的一致性。如果连接的两个字符串中有一个属于数字类型，必须使用`str()`函数将其转为字符串后才能连接。

## 参考资料

- [Python官方文档 - 字符串文本序列](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Python中字符连接的几种方式](https://blog.csdn.net/qq_33185991/article/details/80756204)
- [Python字符串的连接方法](https://www.jianshu.com/p/aa2d81dd3857)

## 参见

- [将多个字符串连接起来的方法](https://www.runoob.com/python/python-strings.html)
- [Python字符串处理](https://www.liaoxuefeng.com/wiki/1016959663602400/1017032075800480)