---
title:                "字符串大写化"
date:                  2024-02-03T19:04:52.218672-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
