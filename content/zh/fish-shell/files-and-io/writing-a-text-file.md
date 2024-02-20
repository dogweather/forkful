---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:45.299688-07:00
description: "\u5728 Fish Shell \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5141\u8BB8\
  \u60A8\u6301\u4E45\u5730\u5B58\u50A8\u6570\u636E\uFF0C\u4F7F\u6570\u636E\u6613\u4E8E\
  \u68C0\u7D22\u6216\u901A\u8FC7\u540C\u4E00\u4E2A Fish \u811A\u672C\u6216\u5176\u4ED6\
  \u7A0B\u5E8F\u8FDB\u884C\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u65E5\u5FD7\u8BB0\u5F55\u3001\u4FDD\u5B58\u914D\u7F6E\u8BBE\u7F6E\u6216\
  \u5BFC\u51FA\u6570\u636E\u4EE5\u8FDB\u884C\u8FDB\u4E00\u6B65\u5904\u7406\u3002"
lastmod: 2024-02-19 22:05:07.344123
model: gpt-4-0125-preview
summary: "\u5728 Fish Shell \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5141\u8BB8\
  \u60A8\u6301\u4E45\u5730\u5B58\u50A8\u6570\u636E\uFF0C\u4F7F\u6570\u636E\u6613\u4E8E\
  \u68C0\u7D22\u6216\u901A\u8FC7\u540C\u4E00\u4E2A Fish \u811A\u672C\u6216\u5176\u4ED6\
  \u7A0B\u5E8F\u8FDB\u884C\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u65E5\u5FD7\u8BB0\u5F55\u3001\u4FDD\u5B58\u914D\u7F6E\u8BBE\u7F6E\u6216\
  \u5BFC\u51FA\u6570\u636E\u4EE5\u8FDB\u884C\u8FDB\u4E00\u6B65\u5904\u7406\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么和为什么？

在 Fish Shell 中写入文本文件允许您持久地存储数据，使数据易于检索或通过同一个 Fish 脚本或其他程序进行操作。程序员这样做是为了日志记录、保存配置设置或导出数据以进行进一步处理。

## 如何操作：

要在 Fish 中写入文本文件，您可以使用 `echo` 命令结合重定向操作符。没有专为 Fish 中的文件写入而流行的第三方库，因为 shell 的内置命令就是简单高效地执行此任务。

### 向新文件写入文本或覆盖现有文件：
```fish
echo "Hello, Fish Shell!" > output.txt
```
此命令将 "Hello, Fish Shell!" 写入 `output.txt`，如果文件不存在则创建，如果存在则覆盖。

### 向现有文件追加文本：
如果您想在不删除其当前内容的情况下向现有文件的末尾添加文本，请使用追加操作符 `>>`：
```fish
echo "Adding new line to file." >> output.txt
```

### 写入多行：
您可以通过使用带有换行符 `\n` 的 echo 来将多行写入文件，或者您可以使用分号将多个 echo 命令链在一起：
```fish
echo "First Line\nSecond Line" > output.txt
# 或
echo "First Line" > output.txt; echo "Second Line" >> output.txt
```

### 示例输出：
要在运行以上命令后查看 `output.txt` 的内容，使用 `cat` 命令：
```fish
cat output.txt
```
```plaintext
First Line
Second Line
```
如上所示替换或追加文本操作文件内容满足您的要求，展示了在 Fish Shell 中处理文本文件的简单而强大的方式。
