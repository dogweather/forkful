---
title:    "Javascript: 将字符串转为小写"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

为什么要将字符串转换为小写？字符串是任何编程语言中的基本数据类型，它表示文本。在某些情况下，我们可能需要将字符串转换成小写。这可能是为了标准化数据，或者为了方便比较字符串。无论原因是什么，了解如何将字符串转换为小写对于有效地操作字符串至关重要。

## 如何操作

要将字符串转换为小写，我们可以使用内置的toLowerCase()方法。这个方法将字符串的所有字母转换为小写，并将其作为新的字符串返回。下面是一个示例：

```Javascript
let str = "HELLO WORLD";
let lowerStr = str.toLowerCase();
console.log(lowerStr);
```

这将输出："hello world"。

## 深入了解

为了更深入地了解如何将字符串转换为小写，我们需要知道字符串是如何存储的。在JavaScript中，字符串是Unicode字符序列，它们不可变，也就是说无法修改。因此，toLowerCase()方法不会改变原始字符串，而是返回一个新的字符串。另外，要注意的是，这个方法只能转换英文字母为小写，其他字符不会有任何改变。

## 参考链接

- MDN Web Docs: String.prototype.toLowerCase() (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- TutorialsPoint: JavaScript toLowerCase() Method (https://www.tutorialspoint.com/javascript/string_tolowercase.htm)
- W3Schools: JavaScript String toLowerCase() Method (https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- FreeCodeCamp: How to Use the toLowerCase() Method in JavaScript (https://www.freecodecamp.org/news/javascript-string-to-lowercase-51eff9281c9c/)