---
date: 2024-01-26 03:42:15.013253-07:00
description: "\u600E\u4E48\u505A\uFF1A Ruby\u4E3A\u526A\u9664\u90A3\u4E9B\u70E6\u4EBA\
  \u7684\u5F15\u53F7\u63D0\u4F9B\u4E86\u4E00\u4E9B\u5DE7\u5999\u7684\u6280\u5DE7\u3002\
  \u4F60\u53EF\u4EE5\u4F7F\u7528`gsub`\u6216`delete`\u65B9\u6CD5\u6765\u5B8C\u6210\
  \u4EFB\u52A1\u3002\u8FD9\u91CC\u6709\u4E00\u4E9B\u4EE3\u7801\u4F9B\u4F60\u53C2\u8003\
  \uFF1A."
lastmod: '2024-04-05T22:38:47.496269-06:00'
model: gpt-4-0125-preview
summary: "\u600E\u4E48\u505A\uFF1A Ruby\u4E3A\u526A\u9664\u90A3\u4E9B\u70E6\u4EBA\u7684\
  \u5F15\u53F7\u63D0\u4F9B\u4E86\u4E00\u4E9B\u5DE7\u5999\u7684\u6280\u5DE7\u3002\u4F60\
  \u53EF\u4EE5\u4F7F\u7528`gsub`\u6216`delete`\u65B9\u6CD5\u6765\u5B8C\u6210\u4EFB\
  \u52A1\u3002\u8FD9\u91CC\u6709\u4E00\u4E9B\u4EE3\u7801\u4F9B\u4F60\u53C2\u8003\uFF1A\
  ."
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

## 怎么做：
Ruby为剪除那些烦人的引号提供了一些巧妙的技巧。你可以使用`gsub`或`delete`方法来完成任务。这里有一些代码供你参考：

```ruby
# 使用 gsub 来移除双引号和单引号
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# 输出：Say hello to my little friend!

# 如果你知道你只会处理一种类型的引号
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# 输出：Stay a while and listen!
```

## 深入探讨
引号的历史可以追溯到编程的最早日子，那时它们经常用作字符串的界定符。现在，正如那时，当引号字符不需要或它们可能会干扰数据存储和操作时，你可能会发现自己需要移除这些引号字符。

我们已经讨论了`gsub`和`delete`，但还有其他方法，如`tr`或`tr_s`，它们可以提供更多控制或能够处理一些不同的用例：

```ruby
# tr 也可以移除引号
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# 输出：Do or do not, there is no try.
```

请记住，每种方法都有其用例。`gsub`在处理复杂模式或多个替换时更为强大。`delete`和`tr`非常适合简单、直接的字符移除。

## 另见
想要额外阅读，并在更大的代码库中看到这些方法的应用，请查看：
- Ruby 文档的[String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub)、[String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete) 和 [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr)。
- Ruby Monstas 有一个很好的[字符串练习集](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html)，其中包括处理引号的内容。
- Stack Overflow 上关于[字符串操作](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string)的讨论提供来自同行 Ruby 开发者的现实问题和解决方案。
