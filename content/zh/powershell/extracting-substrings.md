---
title:                "从计算机编程的角度来看：提取子字符串"
html_title:           "PowerShell: 从计算机编程的角度来看：提取子字符串"
simple_title:         "从计算机编程的角度来看：提取子字符串"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么&为何要做？
提取子字符串是从一个长字符串中提取所需的部分子字符串。程序员通常这样做是因为需要在某些情况下只使用特定的信息或字符，而不是整个字符串。

## 如何做？
提取子字符串可以使用PowerShell中的Substring方法来实现。该方法接受两个参数，分别为开始位置和长度。下面是一个示例代码及输出：

```PowerShell
$str = "Hello world"
$str.Substring(6, 5)
```

输出：world

## 深入了解
提取子字符串的概念早在1954年被提出，但是随着计算机的发展，它的实现方法也不断演变。除了Substring方法外，还有其他方法可以实现提取子字符串的功能，例如使用正则表达式或者字符串截取函数等。在实际应用中，程序员还可以使用字符串拼接的方式来实现一些复杂的提取操作。

## 相关资料
- [PowerShell官方文档](https://docs.microsoft.com/zh-cn/powershell/scripting/samples/working-with-strings?view=powershell-7.1)
- [字符串提取的历史背景](https://www.columbia.edu/itc/mealac/pritchett/00generallinks/hrm/karaka.html)