---
date: 2024-01-20 17:47:44.935616-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u5728JavaScript\u4E2D\uFF0C\u5B57\
  \u7B26\u4E32\u7684\u957F\u5EA6\u662F\u901A\u8FC7 `.length` \u5C5E\u6027\u83B7\u5F97\
  \u7684\u3002\u8FD9\u4E0D\u662F\u4E00\u4E2A\u65B9\u6CD5\uFF0C\u800C\u662F\u4E00\u4E2A\
  \u5C5E\u6027\uFF0C\u6240\u4EE5\u540E\u9762\u4E0D\u9700\u8981\u52A0\u62EC\u53F7\u3002\
  \u4ECE\u5386\u53F2\u4E0A\u6765\u770B\uFF0C`.length`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.483560-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u5728JavaScript\u4E2D\uFF0C\u5B57\u7B26\u4E32\
  \u7684\u957F\u5EA6\u662F\u901A\u8FC7 `.length` \u5C5E\u6027\u83B7\u5F97\u7684\u3002\
  \u8FD9\u4E0D\u662F\u4E00\u4E2A\u65B9\u6CD5\uFF0C\u800C\u662F\u4E00\u4E2A\u5C5E\u6027\
  \uFF0C\u6240\u4EE5\u540E\u9762\u4E0D\u9700\u8981\u52A0\u62EC\u53F7\u3002\u4ECE\u5386\
  \u53F2\u4E0A\u6765\u770B\uFF0C`.length` \u662F\u91C7\u7528\u7684\u8BB8\u591A\u7F16\
  \u7A0B\u8BED\u8A00\u4E2D\u68C0\u6D4B\u5B57\u7B26\u4E32\u957F\u5EA6\u7684\u6807\u51C6\
  \u65B9\u5F0F\u3002\u5FC5\u987B\u6CE8\u610F\u7684\u662F\uFF0C\u5728\u67D0\u4E9B\u8BED\
  \u8A00\uFF0C\u6BD4\u5982utf-16\u4E2D\uFF0C\u4F7F\u7528`.length` \u53EF\u80FD\u5F97\
  \u4E0D\u5230\u6B63\u786E\u7684\u5B57\u7B26\u6570\u91CF\uFF0C\u56E0\u4E3A\u5B83\u8BA1\
  \u7B97\u7684\u662F16\u4F4D\u7801\u5143\u7684\u6570\u91CF\u3002\u5BF9\u4E8EJavaScript\uFF0C\
  \u5B83\u901A\u5E38\u662F\u6309\u5B57\u7B26\u8BA1\u6570\u7684\uFF0C\u4E0E\u5927\u591A\
  \u6570\u73B0\u4EE3\u6587\u672C\u7F16\u8F91\u5668\u7684\u884C\u4E3A\u4E00\u81F4\u3002\
  \u5F53\u7136\uFF0C\u4E5F\u6709\u5176\u4ED6\u65B9\u6CD5\u6765\u68C0\u6D4B\u5B57\u7B26\
  \u4E32\u7684\u957F\u5EA6\uFF0C\u5982\u4F7F\u7528UTF-16\u4EE3\u7801\u5355\u5143\u7684\
  \u6570\u91CF\u6765\u8BA1\u7B97\uFF0C\u4F46\u8FD9\u5728\u65E5\u5E38JavaScript\u7F16\
  \u7A0B\u4E2D\u4E0D\u592A\u5E38\u89C1\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to: (如何操作)
```javascript
let greeting = "你好，世界！";
console.log(greeting.length); // 输出: 7

let emptyString = "";
console.log(emptyString.length); // 输出: 0
```

## Deep Dive (深入了解)
在JavaScript中，字符串的长度是通过 `.length` 属性获得的。这不是一个方法，而是一个属性，所以后面不需要加括号。从历史上来看，`.length` 是采用的许多编程语言中检测字符串长度的标准方式。必须注意的是，在某些语言，比如utf-16中，使用`.length` 可能得不到正确的字符数量，因为它计算的是16位码元的数量。对于JavaScript，它通常是按字符计数的，与大多数现代文本编辑器的行为一致。当然，也有其他方法来检测字符串的长度，如使用UTF-16代码单元的数量来计算，但这在日常JavaScript编程中不太常见。

## See Also (另请参阅)
- JavaScript 字符串参考: [MDN Docs - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- 字符编码细节: [UTF-16](https://en.wikipedia.org/wiki/UTF-16)
- JavaScript字符串操作指南: [字符串的处理](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Text_formatting)
