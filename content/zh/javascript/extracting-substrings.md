---
title:    "Javascript: 提取子字符串"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 为什么要提取子串？

提取子串是一种常用的编程技巧，在处理字符串时非常有用。通过提取子串，您可以从一个字符串中获取特定部分的内容，以便在您的程序中使用。无论是处理用户输入还是处理大量文本数据，提取子串都是非常有用的。

## 如何提取子串？

提取子串可以通过在JavaScript中使用substring()函数来实现。这个函数接受两个参数，第一个参数是开始位置，第二个参数是结束位置。以下是一个简单的示例：

```Javascript
let str = "Hello world";
let substr = str.substring(3, 7);
console.log(substr); // 输出结果为 "lo w"
```

在这个例子中，我们从位置3开始提取子串，直到位置7结束。注意，这个结束位置是不包含在提取的子串中的。您还可以使用负数来代表从右侧开始计算的位置，例如 -3 代表倒数第三个字符。

## 深入了解提取子串

除了substring()函数之外，在JavaScript中还有其他提取子串的方法，比如slice()和substr()。与substring()一样，这些方法也接受开始和结束位置作为参数。它们之间的区别在于，slice()方法接受负数作为开始和结束位置，而substr()方法接受第二个参数作为子串的长度。

此外，您也可以使用正则表达式来提取子串。这种方法可以根据特定的模式来提取需要的内容，非常灵活。若要了解更多关于正则表达式的信息，请参考下面的"相关链接"部分。

## 参考链接

- [JavaScript substring() 函数](https://www.w3schools.com/jsref/jsref_substring.asp)
- [JavaScript slice() 函数](https://www.w3schools.com/jsref/jsref_slice_string.asp)
- [JavaScript substr() 函数](https://www.w3schools.com/jsref/jsref_substr.asp)
- [正则表达式教程](https://www.runoob.com/regexp/regexp-tutorial.html)