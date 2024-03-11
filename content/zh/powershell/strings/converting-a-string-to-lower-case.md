---
date: 2024-01-20 17:38:56.514592-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\
  \u5C0F\u5199\u610F\u5473\u7740\u628A\u6240\u6709\u5B57\u6BCD\u5B57\u7B26\u6539\u4E3A\
  \u5C0F\u5199\u5F62\u5F0F\u3002\u8FD9\u6837\u505A\u901A\u5E38\u4E3A\u4E86\u6BD4\u8F83\
  \u3001\u641C\u7D22\u6216\u6807\u51C6\u5316\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.793562-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\
  \u5C0F\u5199\u610F\u5473\u7740\u628A\u6240\u6709\u5B57\u6BCD\u5B57\u7B26\u6539\u4E3A\
  \u5C0F\u5199\u5F62\u5F0F\u3002\u8FD9\u6837\u505A\u901A\u5E38\u4E3A\u4E86\u6BD4\u8F83\
  \u3001\u641C\u7D22\u6216\u6807\u51C6\u5316\u6570\u636E\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
在编程中，将字符串转换为小写意味着把所有字母字符改为小写形式。这样做通常为了比较、搜索或标准化数据。

## How to: 如何操作
在PowerShell中，你可以使用`.ToLower()`方法来转换字符串。看看下面的例子：

```PowerShell
$originalString = "Hello, World!"
$lowerCaseString = $originalString.ToLower()
$lowerCaseString
```

输出会是：

```
hello, world!
```

## Deep Dive 深入探讨
转换字符串的大小写跟随着编程语言的发展而变得简单。在早期，可能需要逐个检查字符来手动转换，但现代语言提供了内置方法。

除了`.ToLower()`，还有其他一些方法和技术可以用来达到同样的效果。例如，使用正则表达式或ASCII值操作来转换字符。但这些方法通常比`.ToLower()`复杂，所以它是首选的方法。

字符串转换到小写并不是语言特有的；几乎所有编程语言都提供了这个功能。在底层实现上，都依赖于字符编码标准，如ASCII或Unicode，它们规定了大写和小写字母的对应关系。

## See Also 参阅链接
- Unicode 标准: [Unicode Standard](http://www.unicode.org/standard/standard.html)
- 正则表达式快速参考: [Regular Expressions Quick Reference](https://www.regular-expressions.info/refquick.html)
