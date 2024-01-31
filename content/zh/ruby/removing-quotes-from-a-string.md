---
title:                "从字符串中移除引号"
date:                  2024-01-26T03:42:15.013253-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
从字符串中剥离引号意味着去掉包围文本值的那些双引号或单引号。程序员经常这样做来清理用户输入，以确保数据处理的一致性，或为了准备数据给可能会被这些额外字符弄混的系统。

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
