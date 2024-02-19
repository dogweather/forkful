---
aliases:
- /zh/fish-shell/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:20.667548-07:00
description: "\u5728 Fish Shell \u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\
  \u5141\u8BB8\u811A\u672C\u57FA\u4E8E\u76EE\u5F55\u7ED3\u6784\u7684\u5B58\u5728\u6216\
  \u7F3A\u5931\u505A\u51FA\u51B3\u7B56\uFF0C\u4F7F\u5F97\u50CF\u6761\u4EF6\u6587\u4EF6\
  \u64CD\u4F5C\u3001\u65E5\u5FD7\u8BB0\u5F55\u6216\u73AF\u5883\u8BBE\u7F6E\u8FD9\u6837\
  \u7684\u4EFB\u52A1\u6210\u4E3A\u53EF\u80FD\u3002\u8FD9\u4E2A\u6280\u5DE7\u5BF9\u4E8E\
  \u7F16\u5199\u4E0E\u6587\u4EF6\u7CFB\u7EDF\u4EE5\u53EF\u9884\u6D4B\u7684\u65B9\u5F0F\
  \u4EA4\u4E92\u7684\u5065\u58EE\u811A\u672C\u81F3\u5173\u91CD\u8981\u3002"
lastmod: 2024-02-18 23:08:59.532418
model: gpt-4-0125-preview
summary: "\u5728 Fish Shell \u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\
  \u5141\u8BB8\u811A\u672C\u57FA\u4E8E\u76EE\u5F55\u7ED3\u6784\u7684\u5B58\u5728\u6216\
  \u7F3A\u5931\u505A\u51FA\u51B3\u7B56\uFF0C\u4F7F\u5F97\u50CF\u6761\u4EF6\u6587\u4EF6\
  \u64CD\u4F5C\u3001\u65E5\u5FD7\u8BB0\u5F55\u6216\u73AF\u5883\u8BBE\u7F6E\u8FD9\u6837\
  \u7684\u4EFB\u52A1\u6210\u4E3A\u53EF\u80FD\u3002\u8FD9\u4E2A\u6280\u5DE7\u5BF9\u4E8E\
  \u7F16\u5199\u4E0E\u6587\u4EF6\u7CFB\u7EDF\u4EE5\u53EF\u9884\u6D4B\u7684\u65B9\u5F0F\
  \u4EA4\u4E92\u7684\u5065\u58EE\u811A\u672C\u81F3\u5173\u91CD\u8981\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
在 Fish Shell 中检查目录是否存在允许脚本基于目录结构的存在或缺失做出决策，使得像条件文件操作、日志记录或环境设置这样的任务成为可能。这个技巧对于编写与文件系统以可预测的方式交互的健壮脚本至关重要。

## 如何操作：
Fish Shell 使用 `test` 命令来检查文件类型和特性，包括目标是否为目录。这里有一个检查目录是否存在的基本模式：

```fish
if test -d /path/to/dir
    echo "目录存在"
else
    echo "目录不存在"
end
```
示例输出：
```
目录存在
```

为了更流畅的文件和目录操作，人们可能会转向像 `fd` 这样的外部工具，尽管它更常用于查找文件和目录而不仅仅是检查存在性。然而，将其与 Fish 脚本结合起来可以获得便捷的结果：

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "目录存在"
else
    echo "目录不存在"
end
```

这个 `fd` 示例在指定深度搜索目录，而 `grep` 用于检查匹配，使它适用于细致的检查。然而，为了直接检查存在性，坚持使用 Fish 内置的 `test` 既高效又直接。
