---
title:                "删除符合模式的字符"
html_title:           "Ruby: 删除符合模式的字符"
simple_title:         "删除符合模式的字符"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么要删除匹配模式的字符

有时，在处理字符串的时候，我们会遇到需要删除特定字符的情况。这可能是因为这些字符对我们来说没有意义，或者它们可能会干扰我们的代码运行。无论是哪种情况，删除字符匹配模式可以帮助我们更有效地处理字符串数据。

## 如何做到

要删除匹配模式的字符，我们可以使用Ruby中的sub和gsub方法。sub方法用于替换第一个匹配的字符，而gsub方法用于替换所有匹配的字符。下面是一个使用sub方法的例子：

```Ruby
str = "Hello, World!"
new_str = str.sub("o", "")
puts new_str #=> Hell, World!
```

我们可以看到，"o"被成功删除了。

如果我们想要删除所有匹配的字符，可以使用gsub方法，如下所示：

```Ruby
str = "hello world!"
new_str = str.gsub("l", "")
puts new_str #=> heo word!
```

在这里，我们使用gsub来删除所有的“l”字符。

## 深入探讨

除了简单地删除特定字符，我们也可以使用正则表达式来匹配模式。正则表达式是一种强大的工具，可以帮助我们更精确地匹配字符。下面是一个使用正则表达式的例子：

```Ruby
str = "Hello, World!"
new_str = str.gsub(/[o, e]/, "")
puts new_str #=> Hll, Wrld!
```

在这个例子中，我们使用了正则表达式“[o, e]”，它匹配所有“o”和“e”的字符，并将它们替换为空字符串。

## 参考链接

- [Ruby String Methods](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby Regular Expressions](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Ruby Regular Expressions Tutorial](https://www.regular-expressions.info/ruby.html)

## 详见

- [Ruby Docs](https://www.ruby-lang.org/zh_cn/)
- [Ruby China](https://ruby-china.org/)