---
date: 2024-01-26 03:50:03.697954-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8C03\u8BD5\u7684\u6982\u5FF5\u81EA\u8BA1\
  \u7B97\u673A\u65E9\u671F\u4FBF\u5DF2\u5B58\u5728\u2014\u2014\u4F20\u8BF4\u662F\u4ECE\
  1940\u5E74\u4EE3\u53D1\u73B0\u4E00\u53EA\u98DE\u86FE\u5361\u5728\u8BA1\u7B97\u673A\
  \u4E2D\u5F00\u59CB\u7684\uFF01\u4ECA\u5929\uFF0C\u50CF\u5185\u7F6E\u6D4F\u89C8\u5668\
  \u5DE5\u5177\uFF08Chrome\u5F00\u53D1\u8005\u5DE5\u5177\u3001Firefox\u5F00\u53D1\u8005\
  \u5DE5\u5177\uFF09\u6216\u96C6\u6210\u5230IDE\u4E2D\u7684\u8C03\u8BD5\u5668\uFF08\
  Visual Studio Code\u3001WebStorm\uFF09\u63D0\u4F9B\u4E86\u5927\u91CF\u7684\u529F\
  \u80FD\u3002\u2026"
lastmod: '2024-04-05T22:51:01.417173-06:00'
model: gpt-4-0125-preview
summary: "\u5185\u7F6E\u8C03\u8BD5\u5668\u7684\u66FF\u4EE3\u54C1\u5305\u62EC\u7B2C\
  \u4E09\u65B9\u5DE5\u5177\uFF0C\u5982WebStorm\uFF0C\u6216\u4F7F\u7528\u8001\u597D\
  \u4EBA`console.log`\u6765\u8F93\u51FA\u53D8\u91CF\u72B6\u6001\u3002\u4F46\u8FD9\u4E9B\
  \u4E0D\u80FD\u63D0\u4F9B\u50CF\u8C03\u8BD5\u5668\u90A3\u6837\u7684\u5B9E\u65F6\u4E92\
  \u52A8\u548C\u8BE6\u7EC6\u68C0\u67E5\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
这里有一段行为异常的JavaScript代码：

```javascript
function buggyMultiply(a, b) {
    return a + b; // 哎呀！这应该是乘法，而不是加法。
}

let result = buggyMultiply(5, 3);
console.log('结果:', result);
```

输出结果不正确：
```
结果: 8
```

让我们在Chrome开发者工具中调试：

1. 在浏览器中打开这段JS代码。
2. 右击并选择“检查”以打开开发者工具。
3. 点击“Sources”标签页。
4. 找到您的代码片段或页面，并通过点击`return`语句旁边的行号来设置一个断点。
5. 刷新页面触发断点。
6. 查看“作用域”面板，以查看局部变量`a`和`b`。
7. 点击“跳过下一个函数调用”按钮进行单步执行。
8. 在`return`语句中找到错误。
9. 修正代码：
```javascript
function buggyMultiply(a, b) {
    return a * b; // 修正了！
}

let result = buggyMultiply(5, 3);
console.log('结果:', result);
```

更正后的输出：
```
结果: 15
```

## 深入探讨
调试的概念自计算机早期便已存在——传说是从1940年代发现一只飞蛾卡在计算机中开始的！今天，像内置浏览器工具（Chrome开发者工具、Firefox开发者工具）或集成到IDE中的调试器（Visual Studio Code、WebStorm）提供了大量的功能。

内置调试器的替代品包括第三方工具，如WebStorm，或使用老好人`console.log`来输出变量状态。但这些不能提供像调试器那样的实时互动和详细检查。

就实现细节而言，大多数调试器工作方式类似：它们允许您设置断点来暂停执行、逐步执行代码、检查当前变量状态、观察表达式，甚至可以在飞行中操作值以测试不同的场景。

## 参见
- [Google Chrome开发者工具](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla开发者网络 - Firefox调试器](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - 调试](https://code.visualstudio.com/docs/editor/debugging)
