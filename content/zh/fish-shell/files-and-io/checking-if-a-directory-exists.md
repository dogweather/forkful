---
title:                "检查目录是否存在"
aliases:
- /zh/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:20.667548-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
