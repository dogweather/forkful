---
title:    "Javascript: 寻找字符串的长度"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 为什么

字符串长度是编程中一个常见的概念，它表示一个字符串中包含的字符数量。在JavaScript中，我们可以使用内置函数来找到一个字符串的长度。虽然这可能听起来没有什么特别的，但掌握如何找到字符串的长度有助于我们更好地理解字符串的操作和使用。让我们来看一下具体的方法吧。

## 如何操作

首先，我们需要定义一个字符串变量，比如说"Hello World"。然后，我们可以使用JavaScript中的.length函数来找到它的长度。代码示例如下：

```Javascript
var str = "Hello World";
console.log(str.length); //输出：11
```

在上面的例子中，我们定义了一个名为str的字符串变量，并将其赋值为"Hello World"。然后，我们通过使用.length函数来找到字符串的长度，并使用console.log()函数将结果打印出来。在这种情况下，字符串长度为11个字符。

我们也可以将.length函数与条件语句结合使用，来检查一个字符串是否达到了我们期望的长度。比如说，我们可以用它来验证一个密码是否符合要求：

```Javascript
var password = "password123";
if (password.length >= 8) {
  console.log("密码符合要求");
} else {
  console.log("密码不符合要求，请输入至少8个字符");
}
```

在上面的代码中，我们定义了一个名为password的字符串变量，并将其赋值为"password123"。然后，我们使用条件语句来检查它的长度是否大于等于8个字符，如果是，则输出"密码符合要求"，否则输出"密码不符合要求，请输入至少8个字符"。

## 深入了解

在JavaScript中，字符串的长度是保存在一个内部属性中，这个属性也被称为字符串的.length属性。但需要注意的是，这个属性不是针对字符串的，而是针对字符串对象的。这意味着，如果我们使用.length属性来获取一个字符串变量的长度，它会自动将该变量转换为字符串对象，并返回其长度。

我们还需要注意的是，这个长度属性返回的是一个数字，而不是字符串本身。所以在使用时，我们需要注意将其与字符串本身作区分。

## 参考链接

- [JavaScript字符串长度](https://www.runoob.com/jsref/jsref-length.html)
- [JavaScript字符串对象](https://www.runoob.com/jsref/jsref-obj-string.html)
- [MDN文档 - 字符串长度](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/length)

## 参见

- [JavaScript字符串操作](https://www.javascript.com/learn/strings) 
- [为什么需要掌握JavaScript字符串长度](https://blog.bitsrc.io/understanding-javascript-string-length-b92b210a75cd)