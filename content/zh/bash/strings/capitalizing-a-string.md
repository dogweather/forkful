---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:52.218672-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Bash \u6CA1\u6709\u4E13\u95E8\u7528\u4E8E\
  \u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u7684\u5185\u7F6E\u51FD\u6570\
  \uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u53C2\u6570\u6269\u5C55\u6216\u5916\u90E8\
  \u5DE5\u5177\u5982 `awk` \u6765\u5B8C\u6210\u8FD9\u9879\u4EFB\u52A1\u3002\u4EE5\u4E0B\
  \u662F\u51E0\u79CD\u5728 Bash \u4E2D\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\
  \u5199\u7684\u65B9\u6CD5\uFF1A **\u4F7F\u7528\u53C2\u6570\u6269\u5C55\uFF1A** \u6B64\
  \u65B9\u6CD5\u76F4\u63A5\u5728 shell \u4E2D\u64CD\u4F5C\u5B57\u7B26\u4E32\u3002"
lastmod: '2024-04-05T22:38:47.097859-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Bash \u6CA1\u6709\u4E13\u95E8\u7528\u4E8E\
  \u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u7684\u5185\u7F6E\u51FD\u6570\
  \uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u53C2\u6570\u6269\u5C55\u6216\u5916\u90E8\
  \u5DE5\u5177\u5982 `awk` \u6765\u5B8C\u6210\u8FD9\u9879\u4EFB\u52A1\u3002\u4EE5\u4E0B\
  \u662F\u51E0\u79CD\u5728 Bash \u4E2D\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\
  \u5199\u7684\u65B9\u6CD5\uFF1A **\u4F7F\u7528\u53C2\u6570\u6269\u5C55\uFF1A** \u6B64\
  \u65B9\u6CD5\u76F4\u63A5\u5728 shell \u4E2D\u64CD\u4F5C\u5B57\u7B26\u4E32\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
Bash 没有专门用于将字符串首字母大写的内置函数，但你可以使用参数扩展或外部工具如 `awk` 来完成这项任务。以下是几种在 Bash 中将字符串首字母大写的方法：

**使用参数扩展：**

此方法直接在 shell 中操作字符串。

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
输出：
```
Hello world
```

**使用 `awk`：**

`awk` 是一个强大的文本处理工具，可用于将字符串首字母大写，它在大多数类 Unix 操作系统上都可用。

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
输出：
```
Hello world
```

**使用 `sed`：**

对于更传统的方法，可以使用 `sed` 来将字符串首字母大写。但与前面的方法相比，它稍微复杂一些。

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
输出：
```
Hello world
```

这些代码片段演示了如何在 Bash 中将字符串首字母大写，突出了在文本操作时 shell 脚本的灵活性。
