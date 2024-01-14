---
title:    "Ruby: 连接字符串"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# 为什么会想要连接字符串

字符串连接是在编程中很常见的任务，它可以将多个字符串组合成一个大的字符串，从而方便我们在程序中使用。这样做可以简化代码，提高代码的可读性和可维护性。


## 如何连接字符串

在Ruby中，我们可以通过使用加号（+）或concat方法来连接字符串。

下面是一个简单的示例，使用加号（+）来连接两个字符串：

```Ruby
string_1 = "Hello"
string_2 = "world"
puts string_1 + string_2
```

输出：

```
Helloworld
```

我们也可以使用concat方法来连接字符串：

```Ruby
string_1 = "Hello"
string_2 = "world"
puts string_1.concat(string_2)
```

输出：

```
Helloworld
```

## 深入了解字符串连接

除了使用加号（+）和concat方法之外，Ruby还提供了一些其他的方法来处理字符串连接。其中一个是使用interpolation，它可以在字符串中插入变量，从而避免使用加号（+）连接多个字符串。

例如，我们可以通过将变量包含在双引号（"）中，使用#{}来实现插值：

```Ruby
first_name = "John"
last_name = "Doe"
puts "My name is #{first_name} #{last_name}."
```

输出：

```
My name is John Doe.
```

另一个有用的方法是使用String类的join方法，它可以将数组中的多个字符串连接成一个大的字符串。下面是一个示例：

```Ruby
strings = ["Ruby", "programming", "language"]
puts strings.join(" ")
```

输出：

```
Ruby programming language
```

除了上述提到的方法，Ruby还提供了一些其他的字符串连接方法，你可以通过查阅Ruby文档来了解更多信息。

## 参考资料

- [Ruby字符串文档](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby的基本语法](https://www.ruby-lang.org/zh_cn/documentation/quickstart/)

# 参见

- [我的博客](https://www.myblog.com)
- [Ruby on Rails官方文档](https://rubyonrails.org/documentation/)