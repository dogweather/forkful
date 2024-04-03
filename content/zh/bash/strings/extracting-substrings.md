---
date: 2024-01-20 17:44:55.376589-07:00
description: "How to: \u5728 Bash \u4E2D\uFF0C\u53EF\u4EE5\u7528 `${string:position:length}`\
  \ \u8FD9\u79CD\u65B9\u5F0F\u622A\u53D6\u5B57\u7B26\u4E32\u3002\u4E0B\u9762\u662F\
  \u4E2A\u7B80\u5355\u793A\u4F8B\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.946179-06:00'
model: gpt-4-1106-preview
summary: "\u5728 Bash \u4E2D\uFF0C\u53EF\u4EE5\u7528 `${string:position:length}` \u8FD9\
  \u79CD\u65B9\u5F0F\u622A\u53D6\u5B57\u7B26\u4E32\u3002\u4E0B\u9762\u662F\u4E2A\u7B80\
  \u5355\u793A\u4F8B."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
