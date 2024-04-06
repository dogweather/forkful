---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:20.667548-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish Shell \u4F7F\u7528 `test` \u547D\
  \u4EE4\u6765\u68C0\u67E5\u6587\u4EF6\u7C7B\u578B\u548C\u7279\u6027\uFF0C\u5305\u62EC\
  \u76EE\u6807\u662F\u5426\u4E3A\u76EE\u5F55\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u68C0\
  \u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u7684\u57FA\u672C\u6A21\u5F0F\uFF1A."
lastmod: '2024-04-05T21:53:48.557992-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
