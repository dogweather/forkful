---
title:    "Ruby: 寻找字符串的长度"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么
在Ruby编程中，字符串是一种非常重要的数据结构。了解如何计算字符串的长度可以帮助我们更有效地处理和操作字符串，从而提升代码的质量和效率。

# 如何做
```Ruby
# 使用.length方法计算字符串的长度
string = "你好，世界！"
puts string.length
# 输出：6
```
```Ruby
# 使用.size方法计算字符串的长度
string = "Hello, world!"
puts string.size
# 输出：13
```
```Ruby
# 使用.bytesize方法计算字符串的长度（针对特殊字符）
string = "안녕하세요!"
puts string.bytesize
# 输出：15
```

# 深入探究
计算字符串的长度主要是通过计算字符串中的字符数来实现的。但是，在不同编码下，同样的字符可能会占据不同数量的字节。因此，我们可以使用不同的方法来计算字符串的长度，如示例中的`.length`、`.size`和`.bytesize`。

此外，另一个值得注意的问题是空格和换行符也属于字符串的一部分，因此它们也会被计算在内。如果我们想要忽略这些特殊字符，可以使用`.strip`方法来去除字符串中的空格和换行符再计算长度。

# 参考链接
请参考以下链接来了解更多关于计算字符串长度的方法和技巧：
- [Ruby String类文档](https://ruby-doc.org/core-2.6.3/String.html)
- [Ruby中的字符编码及大小写转换](https://www.rubyguides.com/2018/07/ruby-string-encoding/)
- [Ruby Strip方法文档](https://ruby-doc.org/core-2.6.3/String.html#method-i-strip)

# 查看更多
如果您想深入了解Ruby事物，可以参考以下链接：
- [Ruby编程基础知识](https://www.ruby-lang.org/zh_cn/documentation/)
- [Ruby编程教程](https://www.runoob.com/ruby/ruby-tutorial.html)
- [我的个人博客](https://www.example.com)