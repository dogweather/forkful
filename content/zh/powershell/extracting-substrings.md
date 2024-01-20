---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么?

子字符串提取其实就是从一个大字符串中抽取一部分小的字符串。对于程序员来说，用这种方式处理文本数据可以帮助我们更规范程式，提升效率。

## 如何实现：

```PowerShell
#创建一个字符串
$s = "PowerShell字符串处理"

#使用Substring方法提取子字符串
$sub = $s.Substring(11,4)
echo $sub
```

运行上述代码，输出结果应该是：

```PowerShell
处理
```
在这个例子中，我们提取了从11位开始，长度为4的子字符串。

## 深挖

历史背景：PowerShell子字符串提取的标准方法一直是使用Substring()，这个做法可以直接跟.NET和其它编程环境保持一致。

备选方案：除了Substring()，也可以使用-split操作符分割字符串，然后选择结果数组的一部分作为子字符串。

性能实现细节：在处理大量数据时，Substring()将比-split更有效率，因为它直接在原字符串上操作，而-split则需要复制整个字符串。

## 另请参阅

- PowerShell官方文档: [Substring()](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_methods?view=powershell-7.1#substring)
- 更深入子字符串提取的介绍: [Understanding And Working with Strings in PowerShell](https://adamtheautomator.com/powershell-string/)