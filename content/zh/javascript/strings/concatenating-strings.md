---
date: 2024-01-20 17:35:04.525538-07:00
description: "How to (\u600E\u4E48\u505A) \u7528 `+` \u62FC\u63A5\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.196621-06:00'
model: gpt-4-1106-preview
summary: "\u7528 `+` \u62FC\u63A5\uFF1A."
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## How to (怎么做)
用 `+` 拼接：

```javascript
let greeting = "你好" + ", " + "世界!";
console.log(greeting); // 输出: 你好, 世界!
```

用模板字符串（反引号 `）：

```javascript
let user = "小明";
let age = 25;
let welcomeMessage = `欢迎你, ${user}, 你今年 ${age} 岁了。`;
console.log(welcomeMessage); // 输出: 欢迎你, 小明, 你今年 25 岁了。
```

## Deep Dive (深入了解)
字符串拼接有多种方法，`+` 是最直接的。JavaScript ES6 引入了模板字符串，提高了可读性和书写便利性。长期以来，程序员还使用了如 `array.join()` 和 `StringBuilder`（在其他语言中）之类的方法，以应对性能问题和可维护性。在内部，字符串通常是不可变的，这意味着任何拼接操作都会创建一个新字符串，而不是修改原有的字符串。某些浏览器和JavaScript引擎针对字符串拼接进行了优化，但在大量拼接操作时，使用其他方法可能更高效。

## See Also (另请参阅)
- [MDN Web Docs: Template literals (Template strings)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [MDN Web Docs: String.prototype.concat()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [JavaScript Info: Strings](https://javascript.info/string)
