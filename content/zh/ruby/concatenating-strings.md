---
title:                "Ruby: 连接字符串"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么：为什么要进行字符串串联
在编程中，字符串是一种重要的数据类型，它们是由字符组成的序列，在Ruby中，我们可以使用“+”号来连接两个或多个字符串，将它们合并成一个新的字符串。这种串联操作在处理文本数据时非常常用，它可以让我们更容易地创建复杂的文本输出，比如日志记录、文件操作等等。

## 如何：如何在Ruby中进行字符串串联
在Ruby中，我们可以使用“+”号来连接两个字符串，但需要注意的是，被连接的字符串必须是同种数据类型。下面是一个简单的示例代码：

```Ruby
a = "Hello"
b = "World"
c = a + b
puts c
```
输出结果为：
```
HelloWorld
```

我们也可以使用`+=`的操作符来实现同样的功能，如下所示：

```Ruby
a = "Hello"
a += "World"
puts a
```
输出结果为：
```
HelloWorld
```

此外，我们还可以使用`<<`操作符来追加字符串，如下所示：

```Ruby
a = "Hello"
a << " "
a << "World"
puts a
```
输出结果为：
```
Hello World
```

需要注意的是，这三种方式都会改变原来的字符串，如果想保留原来的字符串不变，可以使用`+`创建一个新的字符串，如下所示：

```Ruby
a = "Hello"
b = "World"
c = a + b
puts a
puts b
```
输出结果为：
```
Hello
World
```

## 深入了解
在之前的例子中，我们提到了字符串必须是同种数据类型才可以进行串联，那么什么是“同种数据类型”呢？在Ruby中，两个字符串被认为是同种数据类型的条件是它们的编码方式必须相同。比如，UTF-8和ASCII的编码方式是不同的，如果想要连接这两种编码方式的字符串，就需要先进行转换，否则会出现编码错误。另外，如果想要连接不同数据类型的内容，可以使用`.to_s`方法将其转换为字符串类型。

## 参考链接
- [Ruby字符串文档](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby编码文档](https://ruby-doc.org/core-3.0.0/Encoding.html)
- [Ruby编码示例](https://stackoverflow.com/questions/2988035/how-can-i-declare-a-unicode-string-in-ruby-1-9)