---
date: 2024-01-20 17:35:04.525538-07:00
description: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u628A\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u8FDE\u5728\u4E00\u8D77\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u662F\u4E3A\u4E86\u7EC4\u5408\u6587\u5B57\uFF0C\u521B\u5EFA\u53E5\u5B50\u6216\
  \u52A8\u6001\u6784\u5EFA\u503C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.196621-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u628A\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u8FDE\u5728\u4E00\u8D77\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u662F\u4E3A\u4E86\u7EC4\u5408\u6587\u5B57\uFF0C\u521B\u5EFA\u53E5\u5B50\u6216\
  \u52A8\u6001\u6784\u5EFA\u503C\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

字符串拼接就是把两个或多个字符串连在一起。程序员这么做是为了组合文字，创建句子或动态构建值。

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
