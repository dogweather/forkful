---
title:                "Javascript: 找出字符串的长度"
simple_title:         "找出字符串的长度"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

为什么：字符串是在编程中非常常见的数据类型，因此了解如何计算字符串的长度是非常重要的。无论是检查用户输入的正确性，还是截取字符串的一部分，都需要准确的字符串长度。

如何：在Javascript中，可以使用字符串的length属性来计算字符串的长度。下面是一个简单的示例代码，展示如何获取字符串长度并打印输出：

```javascript
let str = "Hello World";
console.log(str.length); // outputs 11
```

这里我们定义了一个名为"str"的变量，它存储了一个字符串"Hello World"。然后，使用字符串的length属性来获取字符串的长度，并使用console.log()函数打印输出。在上面的示例中，字符串"Hello World"的长度是11个字符，因此输出为11。

深入探讨：字符串的length属性返回的是字符串中字符的数量，包括空格和标点符号在内。它并不区分字符的类型，因此一个汉字或一个英文字母都算作一个字符。同时，length属性也可以用于空字符串，它会返回0作为长度。需要注意的是，length属性是只读的，无法被重新赋值。

另外，如果我们想要获取字符串中某个特定字符的长度，可以使用字符串的charAt()方法。该方法接受一个数字作为参数，表示要获取的字符的索引值（从0开始）。示例代码如下：

```javascript
let str = "Hello World";
console.log(str.charAt(4).length); // outputs 1, as the 5th character is "o"
```

这里我们使用charAt()方法来获取第5个字符，它是一个字母"o"，因此返回的长度为1。

另外，如果需要将字符串中的所有字符都转为小写或大写，可以使用字符串的toLowerCase()和toUpperCase()方法。例如：

```javascript
let str = "Hello World";
console.log(str.toLowerCase()); // outputs "hello world"
console.log(str.toUpperCase()); // outputs "HELLO WORLD"
```

See Also（参考资料）：
- [JavaScript字符串方法](https://www.w3schools.com/js/js_string_methods.asp)
- [JavaScript字符串长度](https://www.javatpoint.com/javascript-string-length)
- [掌握字符串长度计算](https://www.cnblogs.com/DarrenChan/p/11649840.html)