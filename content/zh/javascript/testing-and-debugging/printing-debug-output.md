---
date: 2024-01-20 17:52:43.260044-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u628A\u53D8\u91CF\u3001\u8868\
  \u8FBE\u5F0F\u7684\u503C\u6216\u8BA1\u7B97\u8FC7\u7A0B\u663E\u793A\u51FA\u6765\u7684\
  \u505A\u6CD5\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u6765\u68C0\u67E5\u4EE3\u7801\
  \u6267\u884C\u60C5\u51B5\uFF0C\u627E\u51FA\u95EE\u9898\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.208324-06:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u628A\u53D8\u91CF\u3001\u8868\
  \u8FBE\u5F0F\u7684\u503C\u6216\u8BA1\u7B97\u8FC7\u7A0B\u663E\u793A\u51FA\u6765\u7684\
  \u505A\u6CD5\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u6765\u68C0\u67E5\u4EE3\u7801\
  \u6267\u884C\u60C5\u51B5\uFF0C\u627E\u51FA\u95EE\u9898\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
打印调试输出是把变量、表达式的值或计算过程显示出来的做法。程序员这么做来检查代码执行情况，找出问题。

## How to: (如何操作：)
JavaScript里，`console.log()`是基本工具来调试。简明示例：

```javascript
console.log('Hello, Debugging World!');

let sum = 0;
for (let i = 1; i <= 5; i++) {
    sum += i;
    console.log(`i: ${i}, sum: ${sum}`);
}
```

输出会是：

```
Hello, Debugging World!
i: 1, sum: 1
i: 2, sum: 3
i: 3, sum: 6
i: 4, sum: 10
i: 5, sum: 15
```

## Deep Dive (深入探索)
过去，JavaScript的`alert()`用于调试，但会打断用户体验。`console.log()`成为标准，因其简单、不干扰页面。除`log`，`console`对象有`info`、`warn`、`error`提供不同级别的信息。大型项目，你可能用更复杂的调试工具，比如Chrome DevTools。

实施细节：`console.log`可以打印几乎所有JavaScript类型。在某些环境中运行时（如Node.js），输出可能会被写入到流中，比如`process.stdout`。

## See Also (延伸阅读)
- MDN上关于`console`的文档：[MDN console](https://developer.mozilla.org/zh-CN/docs/Web/API/console)
- Chrome开发者工具：[Chrome DevTools](https://developer.chrome.com/docs/devtools/)
- 关于Node.js调试的指南：[Node.js Debugging Guide](https://nodejs.org/en/docs/guides/debugging-getting-started/)
