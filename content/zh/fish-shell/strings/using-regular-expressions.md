---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:46.668766-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u867D\u7136 Fish Shell \u672C\u8EAB\u6CA1\
  \u6709\u5185\u7F6E\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\u547D\u4EE4\uFF0C\u4F46\u5B83\
  \u6709\u6548\u5730\u4F7F\u7528\u4E86\u652F\u6301\u6B63\u5219\u8868\u8FBE\u5F0F\u7684\
  \u5916\u90E8\u547D\u4EE4\uFF0C\u5982 `grep`\u3001`sed` \u548C `awk`\uFF0C\u5141\u8BB8\
  \u60A8\u5728\u811A\u672C\u4E2D\u52A0\u5165\u6B63\u5219\u8868\u8FBE\u5F0F\u64CD\u4F5C\
  \u3002"
lastmod: '2024-04-05T22:38:47.390232-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u867D\u7136 Fish Shell \u672C\u8EAB\u6CA1\
  \u6709\u5185\u7F6E\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\u547D\u4EE4\uFF0C\u4F46\u5B83\
  \u6709\u6548\u5730\u4F7F\u7528\u4E86\u652F\u6301\u6B63\u5219\u8868\u8FBE\u5F0F\u7684\
  \u5916\u90E8\u547D\u4EE4\uFF0C\u5982 `grep`\u3001`sed` \u548C `awk`\uFF0C\u5141\u8BB8\
  \u60A8\u5728\u811A\u672C\u4E2D\u52A0\u5165\u6B63\u5219\u8868\u8FBE\u5F0F\u64CD\u4F5C\
  \u3002"
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
weight: 11
---

## 如何操作：
虽然 Fish Shell 本身没有内置的正则表达式命令，但它有效地使用了支持正则表达式的外部命令，如 `grep`、`sed` 和 `awk`，允许您在脚本中加入正则表达式操作。

### 使用 `grep` 进行基本模式匹配
搜索文件中匹配某个模式的行：

```fish
grep '^[0-9]+' myfile.txt
```

此命令查找 `myfile.txt` 中以一个或多个数字开始的行。

### 使用 `sed` 进行提取与替换
从文件中提取电话号码：

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

在 `data.txt` 中替换所有 "foo" 为 "bar"：

```fish
sed 's/foo/bar/g' data.txt
```

### 使用 `string` 进行基本正则表达式操作
Fish Shell 的 `string` 命令支持如匹配和替换的简单正则表达式操作：

在字符串中匹配模式：

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
输出：
```
3.1.2
```

将 'fish' 后面的数字替换为 'X.X.X'：

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
输出：
```
Welcome to fish X.X.X
```

### 使用 `awk` 进行高级匹配
打印第一列匹配特定模式的数据的第二列：

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

此命令查找 `datafile` 中第一列以 "a" 开头后跟一个或多个数字的行，并打印第二列。

通过整合这些外部命令，Fish Shell 程序员可以利用正则表达式的全部力量来执行复杂的文本操作任务，增强 shell 的原生能力。
