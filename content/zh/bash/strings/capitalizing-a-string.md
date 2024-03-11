---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:52.218672-07:00
description: "\u5728 Bash \u4E2D\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\
  \u6D89\u53CA\u5230\u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u8F6C\
  \u6362\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\u6301\u5B57\u7B26\u4E32\u7684\u5176\
  \u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u8FD9\u79CD\u6280\u672F\u901A\u5E38\u7528\u4E8E\
  \u683C\u5F0F\u5316\u8F93\u51FA\u6216\u9075\u5B88\u67D0\u4E9B\u7F16\u7801\u89C4\u8303\
  \uFF0C\u8FD9\u4E9B\u89C4\u8303\u8981\u6C42\u67D0\u4E9B\u5B57\u7B26\u4E32\u4EE5\u5927\
  \u5199\u5B57\u6BCD\u5F00\u5934\uFF0C\u4EE5\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u51FA\
  \u4E8E\u6837\u5F0F\u504F\u597D\u3002"
lastmod: '2024-03-11T00:14:21.734597-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Bash \u4E2D\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\
  \u6D89\u53CA\u5230\u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u8F6C\
  \u6362\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\u6301\u5B57\u7B26\u4E32\u7684\u5176\
  \u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u8FD9\u79CD\u6280\u672F\u901A\u5E38\u7528\u4E8E\
  \u683C\u5F0F\u5316\u8F93\u51FA\u6216\u9075\u5B88\u67D0\u4E9B\u7F16\u7801\u89C4\u8303\
  \uFF0C\u8FD9\u4E9B\u89C4\u8303\u8981\u6C42\u67D0\u4E9B\u5B57\u7B26\u4E32\u4EE5\u5927\
  \u5199\u5B57\u6BCD\u5F00\u5934\uFF0C\u4EE5\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u51FA\
  \u4E8E\u6837\u5F0F\u504F\u597D\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Bash 中将字符串首字母大写涉及到将字符串的第一个字符转换为大写，同时保持字符串的其余部分不变。这种技术通常用于格式化输出或遵守某些编码规范，这些规范要求某些字符串以大写字母开头，以提高可读性或出于样式偏好。

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
