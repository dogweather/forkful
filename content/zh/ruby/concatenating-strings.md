---
title:                "Ruby: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

Concatenate（连接）字符串是编程中一个常用但易被忽视的技巧。通过连接不同的字符串，我们可以创建更有用的输出，提高代码的效率和可读性。

## 怎么做

在Ruby中，我们可以使用+运算符来连接两个及以上的字符串。例如：

```Ruby
puts "Hello" + " " + "world"
```

执行以上代码，将会输出：

```Ruby
Hello world
```

我们还可以使用比较特殊的`<<`运算符，该运算符将修改调用它的对象。例如：

```Ruby
greeting = "Hello"
greeting << " world"
puts greeting
```

输出为：

```Ruby
Hello world
```

`+=`运算符也可以用于连接字符串：

```Ruby
greeting = "Hello"
greeting += " world"
puts greeting
```

输出为：

```Ruby
Hello world
```

## 深入了解

除了使用运算符，我们还可以使用Ruby的`concat`方法来连接字符串。该方法会将传入的字符串追加到调用它的字符串末尾：

```Ruby
greeting = "Hello"
greeting.concat(" world")
puts greeting
```

输出为：

```Ruby
Hello world
```

此外，我们还可以使用`join`方法来连接多个字符串。该方法接受一个参数，用来指定连接字符串的分隔符：

```Ruby
greetings = ["Hello", "你好", "こんにちは"]
puts greetings.join(" ")
```

输出为：

```Ruby
Hello 你好 こんにちは
```

## 参考链接

- [Ruby中的字符串操作](https://ruby-doc.org/core-3.0.1/String.html)
- [Ruby字符串的连接方法](https://www.rubyguides.com/2019/12/ruby-string-concatenation/)
- [Ruby中的运算符](https://ruby-doc.org/core-3.0.1/doc/syntax/operators_rdoc.html)

## 请参阅

- [Ruby字符串的使用技巧](https://medium.com/@brianlift/articles-in-ruby-string-6460b668e0da?source=---------8------------------)

感谢阅读本文，希望它对你学习和使用Ruby中的字符串连接有所帮助！