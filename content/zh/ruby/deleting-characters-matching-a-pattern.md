---
title:    "Ruby: 匹配模式的字符删除"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 为什么要删除匹配模式的字符？

在编程中，有时候我们需要对字符串进行操作，特别是删除一些不符合特定模式的字符。例如，在一个字符串中，我们只想保留所有数字，那么就需要删除所有的字母、符号和空格。下面就让我们来学习如何使用 Ruby 来实现这个功能吧！

## 如何操作

使用 Ruby 自带的 `gsub` 方法，我们可以轻松地删除一个字符串中的特定字符。这个方法接受两个参数，第一个是要删除的字符或匹配模式，第二个是要替换成的字符串。例如，我们可以将所有的空格替换成空字符串，从而删除所有的空格。

```Ruby
str = "Hello, World!"
new_str = str.gsub(" ", "")
puts new_str
# Output: Hello,World!
```

在上面的例子中，我们使用 `gsub` 方法，将原字符串中所有的空格替换成空字符串，然后将结果赋值给 `new_str` 变量，并打印出来。

如果我们想要删除所有非数字字符，可以使用正则表达式来匹配所有的数字。正则表达式在 Ruby 中使用 `/ ... /` 来表示，其中 `^` 表示非， `\d` 表示数字。

```Ruby
str = "2a6b!c8d"
new_str = str.gsub(/[^0-9]/, "")
puts new_str
# Output: 268
```

这个例子中，我们先定义了一个正则表达式 `[^0-9]`，表示匹配所有非数字字符。然后将这个正则表达式作为 `gsub` 方法的第一个参数，将所有非数字字符替换成空字符串，最终打印出数字字符串。

## 深入了解

在上面的例子中，我们只是简单地使用了 `gsub` 方法来删除字符。但实际上，这个方法还有很多强大的功能可以使用。比如，我们可以传入一个块来处理每个匹配到的字符，而不是简单地替换成一个固定的字符串。

```Ruby
str = "apple,banana,orange"
new_str = str.gsub(/[^aeiou]/) do |match|
    match.upcase
end
puts new_str
# Output: A bb,An,n grng
```

在上面的例子中，我们先定义了一个正则表达式 `[^aeiou]`，表示匹配所有非元音字母。然后在 `gsub` 方法中传入一个块，该块会对每个匹配到的字符进行处理。在这里，我们将匹配到的字符转换为大写字母，并将结果替换原字符串中的匹配字符。最终输出结果为每个匹配字符都是大写的字符串。

## 参考链接

- [Ruby 官方文档 - String#gsub](https://ruby-doc.org/core-2.6/String.html#method-i-gsub)
- [Ruby 正则表达式教程](https://www.ruanyifeng.com/blog/2006/09/regexp.html)

# 参考资料

- [Ruby 官方文档 - String#gsub](https://ruby-doc.org/core-2.6/String.html#method-i-gsub)
- [Ruby 正则表达式教程](https://www.ruanyifeng.com/blog/2006/09/regexp.html)