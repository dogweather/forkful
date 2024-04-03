---
date: 2024-01-20 17:39:25.556838-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u6307\u5728\u4EE3\u7801\u6267\
  \u884C\u671F\u95F4\u751F\u6210\u7684\u4E0D\u9700\u8981\u957F\u671F\u4FDD\u7559\u7684\
  \u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\
  \u5904\u7406\u6570\u636E\uFF0C\u907F\u514D\u5728\u5185\u5B58\u4E2D\u5360\u7528\u592A\
  \u591A\u7A7A\u95F4\uFF0C\u6216\u8005\u6709\u65F6\u662F\u4E3A\u4E86\u5145\u5F53\u6570\
  \u636E\u4EA4\u6362\u7684\u4E2D\u4ECB\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.984895-06:00'
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u6307\u5728\u4EE3\u7801\u6267\
  \u884C\u671F\u95F4\u751F\u6210\u7684\u4E0D\u9700\u8981\u957F\u671F\u4FDD\u7559\u7684\
  \u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\
  \u5904\u7406\u6570\u636E\uFF0C\u907F\u514D\u5728\u5185\u5B58\u4E2D\u5360\u7528\u592A\
  \u591A\u7A7A\u95F4\uFF0C\u6216\u8005\u6709\u65F6\u662F\u4E3A\u4E86\u5145\u5F53\u6570\
  \u636E\u4EA4\u6362\u7684\u4E2D\u4ECB\u3002."
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## What & Why? (是什么？为什么？)

创建临时文件是指在代码执行期间生成的不需要长期保留的文件。程序员这么做主要是为了处理数据，避免在内存中占用太多空间，或者有时是为了充当数据交换的中介。

## How to: (怎么做：)

在 Bash 中，创建临时文件可以使用 `mktemp` 命令。例子如下：

```Bash
# 创建临时文件
tempfile=$(mktemp)

# 检查文件是否创建成功
if [ -f "$tempfile" ]; then
    echo "Created temporary file: $tempfile"
else
    echo "Failed to create temp file."
    exit 1
fi

# 使用临时文件...
echo "Some data" > "$tempfile"

# 删除临时文件
rm "$tempfile"
```

样例输出：

```
Created temporary file: /tmp/tmp.Iy9R5VADxK
```

## Deep Dive (深入探讨)

`mktemp` 命令在很多 Unix 系统上都有提供，从 UNIX V7 引入。这种方式相比手动创建临时文件更安全，因为它保证了文件名的唯一性，降低了文件冲突的风险。另外，`mktemp` 还可以创建临时目录，使用 `mktemp -d`。

除了 `mktemp`，还有一些其他方法可以创建临时文件，例如直接使用 `/dev/shm` 目录（如果可用），或者自己生成文件名并检查是否已存在。然而，这些方法通常不如 `mktemp` 安全。

在使用临时文件时，程序员通常需要确保脚本结束时删除这些文件，防止临时文件不断积累占用空间。

## See Also (另请参见)

- Bash `mktemp` man page: https://man7.org/linux/man-pages/man1/mktemp.1.html
- Advanced Bash-Scripting Guide - Temporary Files: https://tldp.org/LDP/abs/html/tempfiles.html
- `mktemp` Wikipedia entry: https://en.wikipedia.org/wiki/Mktemp
