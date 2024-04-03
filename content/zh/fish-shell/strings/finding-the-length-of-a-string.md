---
date: 2024-01-20 17:47:13.710459-07:00
description: "How to: \u5728Fish Shell\u4E2D\uFF0C\u83B7\u53D6\u5B57\u7B26\u4E32\u957F\
  \u5EA6\u975E\u5E38\u76F4\u63A5\u3002\u4F60\u53EF\u4EE5\u4F7F\u7528 `string length`\
  \ \u547D\u4EE4\u3002\u770B\u4E0B\u9762\u7684\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.253147-06:00'
model: gpt-4-1106-preview
summary: "\u5728Fish Shell\u4E2D\uFF0C\u83B7\u53D6\u5B57\u7B26\u4E32\u957F\u5EA6\u975E\
  \u5E38\u76F4\u63A5\u3002\u4F60\u53EF\u4EE5\u4F7F\u7528 `string length` \u547D\u4EE4\
  \u3002\u770B\u4E0B\u9762\u7684\u4F8B\u5B50\uFF1A."
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to:
在Fish Shell中，获取字符串长度非常直接。你可以使用 `string length` 命令。看下面的例子：

```Fish Shell
# 举个例子，我们要找到 "你好，世界" 的长度
set str "你好，世界"
string length $str
```

输出将会是：

```
6
```

注意：中文字符通常被认为是一个字符。

## Deep Dive
在Fish Shell的早期版本中，找出字符串的长度可能需要依赖于外部工具，像`wc`命令。Fish Shell `string`这个内建命令提供了一个简单且原生的方法来处理字符串。

比较另一个常用的Shell，如Bash，它需要通过`${#variable}`语法来获取字符串长度。Fish Shell的设计哲学更倾向于直观的命令。

从实现细节来看，Fish Shell处理字符串长度时正确处理了多字节字符。这意味着即使是中文、日文或其他Unicode字符，也都能精确计算长度。

## See Also
- 官方文档中关于`string`命令的更多信息：[Fish Shell string documentation](https://fishshell.com/docs/current/cmds/string.html)
- 其他Shell字符串操作的比较：[Comparing string operations in different shells](https://hyperpolyglot.org/unix-shells#strings)
