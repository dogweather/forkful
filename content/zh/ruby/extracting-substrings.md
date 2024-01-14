---
title:    "Ruby: 提取子字符串"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

提取子字符串是在编程中经常遇到的任务。它可以帮助我们从较长的字符串中提取出我们需要的部分，使得处理数据变得更加简便和高效。

## 如何做

在Ruby中，提取子字符串有多种方法。最基础的方法是使用`slice`方法，它可以使用索引或范围来提取子字符串。比如，我们想要提取`hello world`这个字符串中的`world`部分，可以这样写：

```Ruby
phrase = "hello world"

# 使用索引提取
puts phrase.slice(6..-1) # 输出 "world"

# 使用范围提取
puts phrase.slice(-5..-1) # 输出 "world"
```

在这个例子中，我们使用了字符串的负索引来实现从末尾开始计数。我们也可以使用`slice!`方法来直接修改原始字符串：

```Ruby
puts phrase.slice!(0..5) # 输出 "hello "
puts phrase # 输出 "world"
```

除了`slice`方法，我们还可以使用`[ ]`语法来提取子字符串，与`slice`类似，它也可以使用索引或范围来操作。然而，与`slice`不同的是，`[ ]`语法返回的是新的字符串，而不是修改原始字符串。例如：

```Ruby
phrase = "hello world"

# 使用索引提取
puts phrase[6..-1] # 输出 "world"
puts phrase # 输出 "hello world"，原始字符串不变

# 使用范围提取
puts phrase[-5..-1] # 输出 "world"
puts phrase # 输出 "hello world"，原始字符串不变
```

除了以上两种方法，Ruby还提供了`scan`方法来提取多个子字符串。该方法需要一个正则表达式作为参数，它会按照正则表达式的规则，从原始字符串中匹配出多个子字符串，并返回一个数组。例如：

```Ruby
phrase = "Hello Ruby, I love Ruby"

match_results = phrase.scan(/Ruby/)
puts match_results # 输出 ["Ruby", "Ruby"]

puts match_results.uniq # 输出 ["Ruby"]，去除重复项
```

## 深入讨论

除了以上提到的几种方法，Ruby还提供了许多其他的方法来提取子字符串，例如`sub`、`gsub`、`split`等。每种方法都有自己的特点和用途，可以根据具体情况选择最合适的方法来使用。同时，也可以尝试结合使用不同的方法，灵活处理提取子字符串的需求。

## 参考链接

- [Ruby字符串文档](https://ruby-doc.org/core/String.html)
- [《Ruby基础知识》字符串](https://ruby-china.org/wiki/ruby-tutorial-string)
- [Ruby Regular Expressions](https://ruby-doc.org/core-2.7.1/Regexp.html)

## 参见

- [实例演示：使用Ruby提取子字符串](https://github.com/ruby-china/learn-ruby/blob/master/examples/extract_substrings.rb)
- [Ruby编程学习资源大全](https://github.com/ruby-china/learn-ruby)
- [《Ruby基础知识》相关文章汇总](https://ruby-china.org/wiki/ruby-tutorial)