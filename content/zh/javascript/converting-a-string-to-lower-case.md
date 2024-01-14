---
title:    "Javascript: 将字符串转换为小写"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 为什么要把字符串转换为小写？

在进行编程时，有时候我们需要将字符串统一为小写的形式。这通常是因为我们需要对字符串进行比较，而忽略大小写可能会导致不准确的结果。因此，将字符串转换为小写可以确保我们得到准确的比较结果。另外，小写字符串也更便于处理，可以减少不必要的错误。

## 如何实现字符串转换为小写？

要将字符串转换为小写，我们可以使用JavaScript内置的toLowerCase()方法。该方法会将字符串中的所有大写字母转换为小写，并返回一个新的小写字符串。下面是一个简单的例子：

```javascript
var str = "Hello World!";
var lowerCase = str.toLowerCase();

console.log(lowerCase);
// 输出：hello world!
```

我们也可以将该方法应用到变量上，而不是直接使用字符串。这样可以更灵活地处理不同的字符串。

```javascript
var str1 = "Apple";
var str2 = "apple";

if (str1.toLowerCase() === str2.toLowerCase()) {
  console.log("这两个字符串是相同的！");
} else {
  console.log("这两个字符串不相同！");
}
// 输出：这两个字符串是相同的！
```

## 深入了解字符串转换为小写

除了toUpperCase()方法之外，JavaScript也提供了其它方法来处理字符串大小写转换。比如，我们可以使用正则表达式来匹配并转换字符串中的大写字母。

```javascript
var str = "HeLlo WoRLd!";
var lowerCase = str.replace(/[A-Z]/g, function(char) {
  return String.fromCharCode(char.charCodeAt(0) + 32);
});

console.log(lowerCase);
// 输出：hello world!
```

另外，我们也可以使用ES6新增的String.prototype.normalize()方法来处理Unicode字符的大小写转换。

总的来说，实现字符串转换为小写有多种方法，我们可以根据具体的需求来选择最适合的方式。

## 看也看

- [JavaScript字符串处理方法](https://www.w3schools.com/js/js_string_methods.asp)
- [字符串大小写转换的实现技巧](https://www.geeksforgeeks.org/javascript-string-prototype-tolowercase-function/)
- [String.prototype.normalize()方法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/normalize)