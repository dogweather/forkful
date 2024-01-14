---
title:                "Ruby: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

为什么：当我们在编写程序时，可能需要知道字符串的长度。字符串长度可以帮助我们有效地处理和操作文本数据，因此能够提高程序的效率和准确性。

怎样做：下面是一些示例代码和输出，帮助您理解如何使用Ruby来获得一个字符串的长度：

```Ruby
# 定义一个字符串
str = "大家好，我是一个字符串！"

# 使用.size方法获得字符串的长度
puts str.size
# 输出：11

# 使用.length方法也可以获得字符串的长度
puts str.length
# 输出：11

# 除了使用内置方法，您也可以使用遍历的方式来获得字符串的长度
# 定义一个变量用来存储字符串的长度
length = 0

# 使用.each_char遍历字符串中的每一个字符，并将length变量加一
str.each_char do
  length += 1
end

# 输出字符串的长度
puts length
# 输出：11
```

深入了解：除了上述提到的两种常用方法外，Ruby还提供了其他一些方法来获得字符串的长度。比如，使用.bytesize方法可以得到字符串的字节长度，使用.codepoints方法可以得到字符串的码点数量等等。另外，字符串在Ruby中是以对象的形式存在，因此您也可以使用对象的方法来获得字符串的长度。

参考链接：
- [Ruby官方文档：字符串长度方法](https://ruby-doc.org/core-2.6.3/String.html#method-i-length)
- [Ruby官方文档：字符串对象的方法](https://ruby-doc.org/core-2.6.3/String.html#class-String-label-Methods)
- [Ruby String类的有用方法总结](https://ruby-china.org/topics/11068)

另见：
- [Markdown语法指南](https://www.markdownguide.org/basic-syntax/)
- [Ruby编程入门指南](https://www.ruby-lang.org/en/documentation/)