---
date: 2024-01-20 17:38:44.300450-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) JavaScript \u63D0\u4F9B\u4E86\
  \u4E00\u4E2A\u7B80\u5355\u7684\u65B9\u6CD5 `.toLowerCase()` \u6765\u628A\u5B57\u7B26\
  \u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u5B57\u6BCD\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.191140-06:00'
model: gpt-4-1106-preview
summary: "JavaScript \u63D0\u4F9B\u4E86\u4E00\u4E2A\u7B80\u5355\u7684\u65B9\u6CD5\
  \ `.toLowerCase()` \u6765\u628A\u5B57\u7B26\u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u5B57\
  \u6BCD."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## How to: (如何操作：)
JavaScript 提供了一个简单的方法 `.toLowerCase()` 来把字符串转换成小写字母。

```javascript
let greeting = "Hello, World!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // 输出: "hello, world!"
```

## Deep Dive (深入了解)
在 JavaScript 早期版本中就已经引入了 `.toLowerCase()`，这个方法根据基本的 Unicode 标准将字符串中的所有大写字母转换为相应的小写字母。

替代方案包括使用正则表达式和 `.replace()` 方法，但这通常不必要，因为 `.toLowerCase()` 已经足够高效且易于使用。在实现细节方面，`.toLowerCase()` 会考虑到地区特定的字符映射，这意味着在不同语言环境下，它能正确处理特定字符的大小写转换。

```javascript
let turkishGreeting = "Merhaba DÜNYA";
let lowerCaseTurkishGreeting = turkishGreeting.toLowerCase();
console.log(lowerCaseTurkishGreeting); // 输出: "merhaba dünya"（考虑了土耳其语中的特殊字符）
```

## See Also (另请参见)
- MDN web docs 上的 `.toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Unicode 案例映射: https://www.unicode.org/reports/tr21/tr21-5.html
- JavaScript 正则表达式: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
