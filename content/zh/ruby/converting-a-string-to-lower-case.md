---
title:                "Ruby: 将字符串转换为小写"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要将字符串转换为小写

在编程中，字符串是非常常见的数据类型。有时候，我们会需要将字符串转换为小写形式，这在处理用户输入、比较字符串等情况下非常有用。

# 如何实现字符串转换为小写

```
# 通过使用 downcase 方法，我们可以将一个字符串转换为小写形式。

"Hello, World!".downcase
# => "hello, world!"
```

通过上述例子可以看出，通过调用字符串对象的 downcase 方法，我们可以直接将字符串转换为小写形式。

# 深入了解字符串转换为小写

在 Ruby 中，字符串有一个非常重要的属性：不可变性。这意味着一旦一个字符串被创建，它就无法再被修改。因此，在使用 downcase 方法时，它并不会改变原始的字符串对象，而是返回一个新的字符串对象。

此外，downcase 方法只会将字符转换为小写形式，而不会影响数字或特殊符号。它也可以与其他方法一起使用，比如与 strip 方法结合使用来去除无效的空格。

# 参考链接

[官方 Ruby 文档中的字符串方法介绍](https://ruby-doc.org/core/String.html#method-i-downcase)
[Ruby 教程 - 字符串](https://www.runoob.com/ruby/ruby-strings.html)
[如何在 Ruby 中处理字符串](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-strings/cheatsheet)