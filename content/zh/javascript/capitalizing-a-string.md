---
title:    "Javascript: 将字符串转换为大写"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么要对字符串进行大写

在编程中，我们经常会处理字符串，包括对它们进行格式化和操作。把字符串转换为大写是一个常见的操作，它可以使得我们的输出更加统一和易于阅读。接下来，我们将讨论为什么要对字符串进行大写，以及如何使用JavaScript来实现这一操作。

## 如何实现字符串大写

要对字符串进行大写，我们可以使用JavaScript的内置方法`toUpperCase()`。此方法会将字符串的每个字符转换为大写，并返回一个新的大写字符串。下面是一个简单的例子：

```Javascript
let str = "hello world";
console.log(str.toUpperCase()); // 输出："HELLO WORLD"
```

我们还可以使用循环来逐个字符对字符串进行大写操作。下面是一个使用`for`循环的例子：

```Javascript
let str = "javascript";
let newStr = "";

for (let i = 0; i < str.length; i++) {
  newStr += str[i].toUpperCase();
}

console.log(newStr); // 输出："JAVASCRIPT"
```

## 深入了解字符串大写

在JavaScript中，字符串是不可变的，这意味着我们无法直接修改一个字符串的内容。因此，当我们对一个字符串进行大写操作时，实际上是创建了一个新的字符串，并将它赋值给一个新的变量。这也是为什么在使用`toUpperCase()`方法时，我们需要将返回的值赋给一个变量。

另外，要注意的是，`toUpperCase()`方法只会影响字符串中的字母字符，其他字符（如数字、标点符号等）则不受影响。所以，如果我们需要将字符串中的其他字符也转换为大写，就需要使用循环来逐个字符进行处理。

总的来说，对字符串进行大写操作是一个相对简单的操作，但它能够为我们的编程工作带来很多方便。希望本文能帮助你更好地理解和使用JavaScript中的字符串大写方法。

# 参考链接

- [MDN Web 文档：toUpperCase()方法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [菜鸟教程：JavaScript字符串操作](https://www.runoob.com/js/js-string.html)