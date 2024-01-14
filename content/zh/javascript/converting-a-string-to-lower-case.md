---
title:                "Javascript: 转换字符串为小写"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编写Javascript程序时，经常会遇到需要将字符串转换为小写的情况。这样做可以使得字符串的比较更加精确和方便，同时也可以避免程序错误。

## 如何做

```javascript
// 下面是一个例子：
let str = "Hello World";
console.log(str.toLowerCase());
// 输出：hello world
```

在Javascript中，可以使用`toLowerCase()`方法来将字符串转换为小写。这个方法会返回一个新的字符串，而不会修改原始字符串。另外，这个方法也适用于Unicode字符串和特殊字符。

## 深入探讨

当我们使用`toLowerCase()`方法时，实际上是调用了String对象的原型方法`String.prototype.toLowerCase()`。这个方法会遍历字符串中的每个字符，然后根据ASCII码表将其转换成对应的小写字符。除此之外，这个方法还会考虑特殊字符的转换，比如德文字符的转换。

另外，需要注意的是，在调用`toLowerCase()`方法时，参数必须是一个字符串。如果传入的是一个非字符串类型的值，那么这个值会被强制转换为字符串后再进行操作。

## 参考资料

- [MDN-String.prototype.toLowerCase()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [ECMAScript-String.prototype.toLowerCase()](https://262.ecma-international.org/6.0/#sec-string.prototype.tolowercase)

## 参见

- [Javascript字符串方法大全](https://www.cnblogs.com/zhangtaoArchive/p/6411222.html)
- [Ascii码表](https://www.cnblogs.com/chenying99/archive/2011/10/09/2195139.html)