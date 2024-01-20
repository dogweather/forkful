---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么和为什么？
删除匹配模式的字符是指在编程中，利用特定的“模式”或者规则，去除特定字符串中的某些字符的一种操作。这种操作在数据清理，信息提取，甚至是优化算法中都非常常见，有助于程序员处理和操作复杂的字符串数据。

## 如何做
下面是 Javascript 中删除匹配字符的一些代码样例和执行结果。

```Javascript
// 定义匹配模式，这里是所有的小写字母
let pattern = /[a-z]/g;

// 需要处理的字符串
let str = "Hello123World";

// 删除匹配的字符
let newStr = str.replace(pattern, '');

console.log(newStr);
```
运行上述代码，输出结果应该是 "H123W"。

## 深入了解
删除匹配模式的字符的操作在早期 UNIX 系统的文本处理工具——正则表达式中已经广为使用。在 Javascript 中，我们可以使用 String.replace 方法以及正则表达式来实现这个功能。

当然，除了使用正则表达式之外，还有其他的方法，例如通过 split 和 join 方法组合来实现。这种方法的优势在于我们可以更加灵活地定义匹配模式。

至于实现方式，Javascript 内部会遍历整个字符串，并对每个字符进行匹配。如果该字符满足匹配模式，则会被替换掉（在我们的例子中是删除）。

## 另请参阅
如果你对 Javascript 的正则表达式以及删除匹配字符有更加深入的兴趣，你可以参考以下网站：

1. [MDN Web Docs 上的正则表达式教程](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide/Regular_Expressions)
2. [Javascript String replace 方法的具体用法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/replace)