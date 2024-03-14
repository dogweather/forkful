---
date: 2024-01-26 03:40:18.955661-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u5265\u79BB\u5F15\u53F7\u610F\u5473\u7740\
  \u6446\u8131\u90A3\u4E9B\u53EF\u80FD\u4F1A\u5E72\u6270\u4F60\u7684\u4EE3\u7801\u7684\
  \u8BA8\u538C\u7684\u5F15\u53F7\u7B26\u53F7\uFF0C\u7279\u522B\u662F\u5F53\u4F60\u5728\
  \u89E3\u6790\u6570\u636E\u6216\u6784\u5EFAJSON\u5BF9\u8C61\u65F6\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u8F93\u5165\uFF0C\u907F\u514D\
  \u8BED\u6CD5\u9519\u8BEF\uFF0C\u5E76\u4F7F\u5B57\u7B26\u4E32\u4E0E\u4EE3\u7801\u7684\
  \u5176\u4ED6\u90E8\u5206\u66F4\u597D\u5730\u534F\u4F5C\u3002"
lastmod: '2024-03-13T22:44:48.192451-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u5265\u79BB\u5F15\u53F7\u610F\u5473\u7740\
  \u6446\u8131\u90A3\u4E9B\u53EF\u80FD\u4F1A\u5E72\u6270\u4F60\u7684\u4EE3\u7801\u7684\
  \u8BA8\u538C\u7684\u5F15\u53F7\u7B26\u53F7\uFF0C\u7279\u522B\u662F\u5F53\u4F60\u5728\
  \u89E3\u6790\u6570\u636E\u6216\u6784\u5EFAJSON\u5BF9\u8C61\u65F6\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u8F93\u5165\uFF0C\u907F\u514D\
  \u8BED\u6CD5\u9519\u8BEF\uFF0C\u5E76\u4F7F\u5B57\u7B26\u4E32\u4E0E\u4EE3\u7801\u7684\
  \u5176\u4ED6\u90E8\u5206\u66F4\u597D\u5730\u534F\u4F5C\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 是什么 & 为什么?
从字符串中剥离引号意味着摆脱那些可能会干扰你的代码的讨厌的引号符号，特别是当你在解析数据或构建JSON对象时。程序员这样做是为了清理输入，避免语法错误，并使字符串与代码的其他部分更好地协作。

## 如何操作:
想象一下，你有一个被双引号包裹的字符串，就像`"\"你好，世界！\""`，你想要纯净的、未加引号的文本。这里有一个快速的JavaScript代码片段，可以解放你的字符串，脱离引号的束缚：

```javascript
let quotedString = "\"你好，世界！\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // 输出: 你好，世界！
```

如果你处理的是单引号怎么办？只需稍微修改一下正则表达式：

```javascript
let singleQuotedString = "'你好，世界！'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // 输出: 你好，世界！
```

或者如果你的字符串是双引号和单引号的混合体呢？没问题：

```javascript
let mixedQuotedString = "\"'你好，世界！'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // 输出: '你好，世界！'
```

## 深入探讨
在JSON成为主流之前，转义引号是一种使用反斜杠和技巧的狂野西部方式。早期的编程语言并不总是能很好地处理引号，这意味着需要大量的手动字符串操作。现在，随着标准化的数据格式的出现，去除引号通常是关于在它们被作为JSON处理或存储文本时，清理输入以避免格式冲突。

`.replace()`的替代方法？当然！你可以在引号上分割和加入字符串，如果你对引号的位置很确定，可以使用slice，或者甚至使用正则表达式匹配来提取所需的文本。这完全取决于上下文。

但不要忘记边缘情况：引号内的引号、转义的引号和国际字符。将你的字符串视为一个充满异常的潜在雷区，并小心行事。现代JavaScript引擎针对正则表达式操作进行了优化，因此它们通常是首选，但总是值得检查性能以便处理重数据处理任务。

## 另请参阅
深入了解字符串操作和正则表达式：

- Mozilla开发者网络关于String.replace()的文章：https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101以测试你的正则表达式模式：https://regex101.com/
- JSON.org了解为什么在现代Web开发中我们要处理这么多引号：http://json.org/
