---
title:    "Ruby: 将字符串转换为小写"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 为什么

许多程序员都会遇到一种情况，在编程过程中需要将一个字符串转换为小写字母，那么为什么我们需要这样做呢？通过将字符串转换为小写字母，我们可以统一处理用户输入的不同大小写形式，从而方便进行字符串比较和匹配。

## 如何操作

要将字符串转换为小写字母，在Ruby中有一个非常简单的方法，那就是使用 `.downcase` 方法。

```Ruby
string = "RUBY"
puts string.downcase

# 输出结果为
ruby
```

除了使用 `.downcase` 方法外，我们也可以使用 `.scan` 方法结合正则表达式来将字符串中的大写字母全部替换为小写字母。

```Ruby
string = "Hello WORLD!"
puts string.scan(/[A-Z]/).join("").downcase

# 输出结果为
helloworld
```

## 深入探讨

当我们调用 `.downcase` 方法时，Ruby会将字符串中所有的大写字母转换为小写字母，并返回一个新的字符串对象，原来的字符串不会被改变。这也是Ruby中的**不可变对象**（immutable object）的概念，意味着我们无法直接修改原始的字符串，而是会创建一个新的字符串对象进行操作。

此外，如果我们使用 `.downcase!` 方法，那么原始的字符串会被直接改变。下面是一个示例：

```Ruby
string = "Hello WORLD!"
string.downcase!
puts string

# 输出结果为
hello world!
```

## 参考资料

在探讨字符串转换为小写字母的过程中，我们也涉及到了正则表达式的使用，以及Ruby中的不可变对象的概念。下面是一些相关的参考资料，供大家进一步学习和探索：

- Ruby官方文档：[`.downcase`](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)、[`.downcase!`](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase-21)、[`.scan`](https://ruby-doc.org/core-2.7.1/String.html#method-i-scan)
- [Ruby正则表达式教程](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
- [Ruby中的不可变对象](https://www.rubyguides.com/2018/11/ruby-objects-mutable-immutability/)
- [惰性求值（lazy evaluation）](https://www.geeksforgeeks.org/lazy-evaluation-in-ruby/)
- [为什么Ruby中的字符串是不可变的？](https://stackoverflow.com/questions/5604852/why-are-ruby-strings-immutable/5605571#5605571)

## 另请参阅

- [Ruby中的其他字符串操作方法](https://www.rubyguides.com/2019/01/ruby-string-methods/)
- [Ruby编程语言入门指南](https://zhuanlan.zhihu.com/p/25329549)
- [Ruby编程语言基础入门教程](https://www.runoob.com/ruby/ruby-tutorial.html)