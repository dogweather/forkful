---
title:                "将字符串大写"
html_title:           "Ruby: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为大写

有时候，我们需要在Ruby程序中将字符串转换为大写。这可能是为了统一格式、方便比较或者满足用户的要求。无论是什么原因，使用Ruby的capitalize方法可以轻松实现这一目的。

## 如何将字符串转换为大写

为了将字符串转换为大写，我们可以使用capitalize方法。下面是一个简单的例子：

```Ruby
str = "hello world"
str.capitalize # 将输出 "Hello world"
```

我们也可以直接在定义字符串的时候就将它转换为大写：

```Ruby
str = "hello world".capitalize # 这样str的值将为 "Hello world"
```

还可以使用upcase方法将整个字符串转换为大写：

```Ruby
str = "hello world"
str.upcase # 将输出 "HELLO WORLD"
```

注意，capitalize和upcase方法都不会修改原始字符串的值，而是返回一个新的字符串。如果希望修改原始字符串的值，可以使用```capitalize!```和```upcase!```这两个带有感叹号的方法。

## 深入了解字符串转换为大写

在Ruby中，字符串是可变的数据类型，这意味着我们可以随意修改字符串的值。然而，capitalize方法只会将字符串的第一个字符转换为大写，而不会对其他字符做出任何改变。如果希望将整个字符串的每个单词的首字母都转换为大写，可以使用capitalize方法的变体——capitalize_words，它可以从第一个单词开始，逐个将每个单词的首字母转换为大写。

另外，如果需要忽略字符串中的特殊字符和数字，只将字母进行大小写转换，可以使用capitalize_letters方法。

总的来说，Ruby中有很多字符串转换的方法，可以根据具体需求选择合适的方法进行字符串转换。

## 链接

- [Ruby官方文档：字符串转换方法](https://ruby-doc.org/core-2.7.2/String.html)
- [资料库：Ruby字符串转换教程](https://www.runoob.com/ruby/ruby-string-capitalization.html)
- [博客文章：深入理解Ruby中的字符串转换](https://www.selleski.com/learning_ruby/string_functions/capitalize/)

## 参考资料

https://ruby-doc.org/core-2.7.2/String.html
https://www.runoob.com/ruby/ruby-string-capitalization.html
https://www.selleski.com/learning_ruby/string_functions/capitalize/