---
title:                "寻找字符串的长度"
html_title:           "Javascript: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么要找到字符串的长度？

在编写JavaScript程序时，字符串是一个常见的数据类型。计算字符串的长度是必要的，它可以帮助我们确定字符串的大小和特征。举个例子，我们可能需要检查字符串的长度是否符合某个特定的限制，或者根据字符串的长度来进行不同的操作。因此，学习如何找到字符串的长度是非常重要的。

## 如何执行

在JavaScript中，计算字符串的长度非常简单。我们可以使用内置的属性 `length` 来获取字符串的长度。让我们看一个简单的例子：

```Javascript
const str = "我爱JavaScript"
console.log(str.length);
// output: 9
```

在这个例子中，字符串 "我爱JavaScript" 的长度被打印为 9。这是因为该字符串由 9 个字符组成（包括汉字和空格）。我们也可以使用 `length` 属性来检查空字符串的长度：

```Javascript
const emptyStr = "";
console.log(emptyStr.length);
// output: 0
```

我们可以看到，空字符串的长度为 0，这是因为它不包含任何可见的字符。

## 深入了解

计算字符串长度的方法背后的原理是，`length` 属性会返回字符串中字符的总数，包括空格和标点符号。这也意味着，字符串的长度和它的真实长度不一定相同。让我们看一个例子：

```Javascript
const str = "Hello, 世界";
console.log(str.length);
// output: 9
```

在这个例子中，字符串 "Hello, 世界" 的长度打印为 9。但实际上，它包含 8 个字符和一个逗号，因此真正的长度为 8。这是因为 `length` 属性只计算字符的数量，而不考虑字符的宽度。

此外，这个方法也适用于数组。我们可以使用 `length` 属性来获取数组中元素的数量。

See Also:

- [MDN - String length property](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN - Array length property](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length)