---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 是什么以及为什么？
字符串插值是一种编程表达手法，使得在字符串中嵌入变量或表达式成为可能。程序员使用字符串插值，主要是为了更清楚地组织和格式化信息输出。

## 怎么做：

在PowerShell中，我们通过`$()`结构来插值：

```PowerShell
$name = "Alice"
echo "Hello, $($name)"
```

输出：

```PowerShell
Hello, Alice
```

你也可以在插入点执行更复杂的表达式：

```PowerShell
$n = 7
echo "The square of $($n) is $($n*$n)"
```

输出：

```PowerShell
The square of 7 is 49
```

## 深入了解：

字符串插值一种历史悠久的编程实践，存在于许多其他编程语言中，例如Perl和Ruby。在PowerShell中，除了上面提到的`$()`结构外，还有类似`${var}`的插值语法，这两者在效果上是一样的。

```PowerShell
$name = "Alice"
echo "Hello, ${name}"
```

输出：

```PowerShell
Hello, Alice
```

然而，`$()`结构可以进行更复杂的表达式评估，比如运算、函数调用等，因此相对灵活度更高。

## 参考资料：
- 查阅[微软官方文档](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.1)了解PowerShell字符串插值的更多细节和例子。
- 加深理解，你可以参考[这篇文章](https://kevinswiber.com/2012/07/23/string-interpolation-with-powershell/)揭示的PowerShell字符串插值详细运作原理。