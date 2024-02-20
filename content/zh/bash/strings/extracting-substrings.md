---
date: 2024-01-20 17:44:55.376589-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u662F\u4ECE\u4E00\u4E32\u6587\u5B57\
  \u4E2D\u53D6\u51FA\u90E8\u5206\u5185\u5BB9\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\
  \u7ECF\u5E38\u8FD9\u4E48\u505A\u4EE5\u83B7\u53D6\u5173\u952E\u6570\u636E\uFF0C\u7B80\
  \u5316\u5904\u7406\u6D41\u7A0B\uFF0C\u6216\u8005\u8C03\u6574\u6587\u672C\u683C\u5F0F\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.994618
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u662F\u4ECE\u4E00\u4E32\u6587\u5B57\
  \u4E2D\u53D6\u51FA\u90E8\u5206\u5185\u5BB9\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\
  \u7ECF\u5E38\u8FD9\u4E48\u505A\u4EE5\u83B7\u53D6\u5173\u952E\u6570\u636E\uFF0C\u7B80\
  \u5316\u5904\u7406\u6D41\u7A0B\uFF0C\u6216\u8005\u8C03\u6574\u6587\u672C\u683C\u5F0F\
  \u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why?
提取子字符串是从一串文字中取出部分内容的过程。程序员经常这么做以获取关键数据，简化处理流程，或者调整文本格式。

## How to:
在 Bash 中，可以用 `${string:position:length}` 这种方式截取字符串。下面是个简单示例。

```Bash
#!/bin/bash
str="Hello, Mandarin readers!"
substring=${str:7:7}
echo $substring
```

运行之后，你会得到输出：

```Bash
Mandarin
```

## Deep Dive
在历史上，Bash 从最初的版本就支持基本的字符串操作。字符串提取是 Bash 脚本中非常重要的功能，帮助我们处理和解析数据。如果不想用 Bash 的内置功能，还有其他替代方案，比如 `cut`, `awk`, `sed`。对于提取子字符串，Bash 内置的方法通常是最直接和快速的，因为它不需要调用额外的命令或者创建新的进程。

## See Also
- Bash 字符串操作官方文档: [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- 更多 `cut`, `awk`, `sed` 示例: [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html)
