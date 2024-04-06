---
date: 2024-01-20 17:35:04.525538-07:00
description: "How to (\u600E\u4E48\u505A) \u5B57\u7B26\u4E32\u62FC\u63A5\u6709\u591A\
  \u79CD\u65B9\u6CD5\uFF0C`+` \u662F\u6700\u76F4\u63A5\u7684\u3002JavaScript ES6 \u5F15\
  \u5165\u4E86\u6A21\u677F\u5B57\u7B26\u4E32\uFF0C\u63D0\u9AD8\u4E86\u53EF\u8BFB\u6027\
  \u548C\u4E66\u5199\u4FBF\u5229\u6027\u3002\u957F\u671F\u4EE5\u6765\uFF0C\u7A0B\u5E8F\
  \u5458\u8FD8\u4F7F\u7528\u4E86\u5982 `array.join()` \u548C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.404005-06:00'
model: gpt-4-1106-preview
summary: "How to (\u600E\u4E48\u505A) \u5B57\u7B26\u4E32\u62FC\u63A5\u6709\u591A\u79CD\
  \u65B9\u6CD5\uFF0C`+` \u662F\u6700\u76F4\u63A5\u7684\u3002JavaScript ES6 \u5F15\u5165\
  \u4E86\u6A21\u677F\u5B57\u7B26\u4E32\uFF0C\u63D0\u9AD8\u4E86\u53EF\u8BFB\u6027\u548C\
  \u4E66\u5199\u4FBF\u5229\u6027\u3002\u957F\u671F\u4EE5\u6765\uFF0C\u7A0B\u5E8F\u5458\
  \u8FD8\u4F7F\u7528\u4E86\u5982 `array.join()` \u548C `StringBuilder`\uFF08\u5728\
  \u5176\u4ED6\u8BED\u8A00\u4E2D\uFF09\u4E4B\u7C7B\u7684\u65B9\u6CD5\uFF0C\u4EE5\u5E94\
  \u5BF9\u6027\u80FD\u95EE\u9898\u548C\u53EF\u7EF4\u62A4\u6027\u3002\u5728\u5185\u90E8\
  \uFF0C\u5B57\u7B26\u4E32\u901A\u5E38\u662F\u4E0D\u53EF\u53D8\u7684\uFF0C\u8FD9\u610F\
  \u5473\u7740\u4EFB\u4F55\u62FC\u63A5\u64CD\u4F5C\u90FD\u4F1A\u521B\u5EFA\u4E00\u4E2A\
  \u65B0\u5B57\u7B26\u4E32\uFF0C\u800C\u4E0D\u662F\u4FEE\u6539\u539F\u6709\u7684\u5B57\
  \u7B26\u4E32\u3002\u67D0\u4E9B\u6D4F\u89C8\u5668\u548CJavaScript\u5F15\u64CE\u9488\
  \u5BF9\u5B57\u7B26\u4E32\u62FC\u63A5\u8FDB\u884C\u4E86\u4F18\u5316\uFF0C\u4F46\u5728\
  \u5927\u91CF\u62FC\u63A5\u64CD\u4F5C\u65F6\uFF0C\u4F7F\u7528\u5176\u4ED6\u65B9\u6CD5\
  \u53EF\u80FD\u66F4\u9AD8\u6548\u3002"
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
