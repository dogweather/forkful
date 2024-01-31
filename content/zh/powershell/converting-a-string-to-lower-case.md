---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:38:56.514592-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

category:             "PowerShell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/converting-a-string-to-lower-case.md"
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
