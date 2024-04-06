---
date: 2024-01-20 17:38:56.514592-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C \u5728PowerShell\u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528`.ToLower()`\u65B9\u6CD5\u6765\u8F6C\u6362\u5B57\u7B26\u4E32\u3002\
  \u770B\u770B\u4E0B\u9762\u7684\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.295126-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C \u5728PowerShell\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\
  \u7528`.ToLower()`\u65B9\u6CD5\u6765\u8F6C\u6362\u5B57\u7B26\u4E32\u3002\u770B\u770B\
  \u4E0B\u9762\u7684\u4F8B\u5B50\uFF1A."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
