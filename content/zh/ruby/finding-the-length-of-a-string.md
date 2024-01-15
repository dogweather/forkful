---
title:                "计算字符串的长度"
html_title:           "Ruby: 计算字符串的长度"
simple_title:         "计算字符串的长度"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

当你编写Ruby程序时，有时需要知道字符串的长度。比如说，你想要限制用户输入的字符数量，或者你需要通过字符串的长度来进行一些计算。在这篇文章中，我们将学习如何使用Ruby来找到字符串的长度。

## 怎么做

```Ruby
# 先定义一个字符串
str = "你好，世界！"

# 使用String类的length方法来获取字符串的长度
puts str.length
#=> 6
```

在上面的例子中，我们定义了一个字符串并赋值给变量`str`，然后通过调用`length`方法来获取字符串的长度，并用`puts`打印到屏幕上。如你所见，方法返回的值为该字符串中字符的数量。

现在让我们来看一个更实际的例子，假设我们要限制用户输入的密码长度为8位，我们可以使用一个if语句来检查输入的密码长度：

```Ruby
puts "请输入您的密码："
password = gets.chomp  #获取用户输入并去掉结尾的换行符
if password.length == 8
	puts "密码设置成功！"
else
	puts "密码长度必须为8位！"
end
```

在这个例子中，我们通过`length`方法来获取用户输入的密码的长度，并通过if语句来判断输入的密码是否符合要求。

## 深入了解

在Ruby中，每个字符串都是一个对象，它们共享`String`类的方法。`length`方法实际上是`String`类的一个实例方法，它返回接收这个方法的字符串的长度。它的工作原理是通过迭代字符串中的每个字符来计算长度。所以如果你有一个很长的字符串，计算它的长度可能会比较耗时。

除了`length`方法，`String`类还有一个`size`方法也可以用来获取字符串的长度，它们的作用是一样的。

## 参考链接

- [Ruby的String类文档](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby的length方法介绍](https://www.rubyguides.com/2019/08/ruby-string-length/#:~:text=The%20length%20method%20in%20Ruby,contains%20the%20finer%20details%20aspects.)
- [通过Ruby中的length方法来计算字符串的长度](https://mentalized.net/journal/2011/03/31/dont_ask_get_with_ruby_question_mark_method/#:~:text=length%20is%20an%20instance%20that%20is%20actually%20implemented%20in%20C.)