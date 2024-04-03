---
date: 2024-01-26 03:50:03.697954-07:00
description: "\u8FD9\u91CC\u6709\u4E00\u6BB5\u884C\u4E3A\u5F02\u5E38\u7684JavaScript\u4EE3\
  \u7801\uFF1A ```javascript function buggyMultiply(a, b) { return a + b; // \u54CE\
  \u5440\uFF01\u8FD9\u5E94\u8BE5\u662F\u4E58\u6CD5\uFF0C\u800C\u4E0D\u662F\u52A0\u6CD5\
  \u3002 } let result = buggyMultiply(5, 3);\u2026"
lastmod: '2024-03-13T22:44:48.211087-06:00'
model: gpt-4-0125-preview
summary: "\u8FD9\u91CC\u6709\u4E00\u6BB5\u884C\u4E3A\u5F02\u5E38\u7684JavaScript\u4EE3\
  \u7801\uFF1A\n\n```javascript\nfunction buggyMultiply(a, b) {\n    return a + b;\
  \ // \u54CE\u5440\uFF01\u8FD9\u5E94\u8BE5\u662F\u4E58\u6CD5\uFF0C\u800C\u4E0D\u662F\
  \u52A0\u6CD5\u3002\n}\n\nlet result = buggyMultiply(5, 3);\nconsole."
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
