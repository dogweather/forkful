---
title:                "将字符串转换为大写"
html_title:           "Ruby: 将字符串转换为大写"
simple_title:         "将字符串转换为大写"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##什么是资本化字符串及为什么要这么做？

资本化字符串是指将某个字符串中的每个字母首字母改为大写形式。程序员这样做的原因是为了让字符串的显示更加美观和易于阅读。

##如何实现：

以下是使用Ruby编程语言实现资本化字符串的两种方式，以及样本输出：

```ruby
#方法一：使用capitalize方法
#输入字符串
str = "hello, world!"
#将字符串的首字母大写
str.capitalize
#输出结果为 "Hello, world!"

#方法二：使用upcase和slice方法
#输入字符串
str = "hello, world!"
#将字符串的首字母大写
str[0] = str[0].upcase
#输出结果为 "Hello, world!"
```

##深入了解：

1. 历史背景：资本化字符串的概念最初来源于编程语言内部的函数，用于处理字符串的格式。随着时代的发展，它也被广泛应用于文本编辑和文档处理软件中。

2. 其他替代方案：除了Ruby中的capitalize方法和upcase、slice组合，也可以使用正则表达式来实现资本化字符串。

3. 实现细节：当资本化一个字符串时，Ruby会根据当前字符串中所有的单词首字母进行大写转换，并将其他字母转换为小写形式。因此，如果要实现特定单词的大写形式，需要使用capitalize方法；如果要实现全大写形式，可使用upcase和slice方法。

##相关阅读：

- [Ruby String#capitalize method](https://rubyapi.org/3.0/o/string#method-i-capitalize)
- [Ruby String#upcase method](https://rubyapi.org/3.0/o/string#method-i-upcase)
- [Ruby String#slice method](https://rubyapi.org/3.0/o/string#method-i-slice)
- [Regular Expressions in Ruby](https://www.rubyguides.com/ruby-tutorial/ruby-regex/)