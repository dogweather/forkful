---
title:    "Javascript: 寻找字符串的长度"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，经常会需要计算字符串的长度。字符串的长度是指字符串中字符的个数。通过计算字符串的长度，我们可以更好地处理和操作字符串，使代码更加可靠和灵活。因此，学习如何找到字符串的长度是非常重要的。

# 如何进行

要计算字符串的长度，我们可以使用字符串的"length"方法。这个方法会返回字符串中字符的个数。让我们看一个例子：

```Javascript
//创建一个字符串
let str = "Hello World!"

//使用length方法计算字符串的长度
let length = str.length

//打印输出字符串的长度
console.log("字符串的长度是：" + length)

//输出结果："字符串的长度是：12"
```

如上所示，我们可以通过"length"方法轻松地计算字符串的长度。请注意，空格和标点符号也会被计算在字符串的长度中。

# 深入了解

虽然"length"方法是我们计算字符串长度的主要方式，但它并不适用于所有情况。一些特殊的字符，比如表情符号或Unicode字符，有可能会被误算为多个字符。这时，我们就需要使用特殊的方法来正确计算字符串的长度。

另外，如果我们想要计算一个中文字符的长度，在Javascript中，它会被算作两个字符。这是因为Javascript默认使用Unicode编码，中文字符的编码范围是在["u4e00", "u9fa5"]，每个中文字符占用两个字节的编码空间。

# 另见

- [String length method - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Special characters and Unicode - W3Schools](https://www.w3schools.com/js/js_special_characters.asp)
- [Unicode and UTF-8 - Freecodecamp](https://www.freecodecamp.org/news/unicode-utf-8-and-javascript-getting-started-805d77f2852f/)