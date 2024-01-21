---
title:                "打印调试输出"
date:                  2024-01-20T17:52:43.260044-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/printing-debug-output.md"
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