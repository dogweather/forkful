---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:30.126773-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728 Fish Shell \u4E2D\uFF0C\u53EF\u4EE5\
  \u76F4\u63A5\u4F7F\u7528\u5185\u7F6E\u51FD\u6570\u6765\u64CD\u4F5C\u5B57\u7B26\u4E32\
  \uFF0C\u65E0\u9700\u5916\u90E8\u5DE5\u5177\u6216\u5E93\u3002\u8981\u4F7F\u5B57\u7B26\
  \u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u4F60\u53EF\u4EE5\u7ED3\u5408\u4F7F\u7528\
  \ `string` \u547D\u4EE4\u53CA\u5176\u5B50\u547D\u4EE4\u3002"
lastmod: '2024-03-13T22:44:48.244801-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Fish Shell \u4E2D\uFF0C\u53EF\u4EE5\u76F4\u63A5\u4F7F\u7528\u5185\
  \u7F6E\u51FD\u6570\u6765\u64CD\u4F5C\u5B57\u7B26\u4E32\uFF0C\u65E0\u9700\u5916\u90E8\
  \u5DE5\u5177\u6216\u5E93\u3002\u8981\u4F7F\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\
  \u5199\uFF0C\u4F60\u53EF\u4EE5\u7ED3\u5408\u4F7F\u7528 `string` \u547D\u4EE4\u53CA\
  \u5176\u5B50\u547D\u4EE4."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作:
在 Fish Shell 中，可以直接使用内置函数来操作字符串，无需外部工具或库。要使字符串首字母大写，你可以结合使用 `string` 命令及其子命令。

```fish
# 示例字符串
set sample_string "hello world"

# 大写首字母
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

输出:
```
Hello world
```

对于需要将字符串中多个单词首字母大写的情况（例如，将 "hello world" 转换为 "Hello World"），你需要对每个单词进行迭代，对每个单词应用首字母大写的逻辑：

```fish
# 示例句子
set sentence "hello fish shell programming"

# 每个单词首字母大写
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# 连接首字母大写的单词
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

输出:
```
Hello Fish Shell Programming
```

注意，Fish Shell 没有直接提供一种像某些编程语言那样通过其字符串方法实现整个句子首字母大写的单命令方式。因此，结合使用 `string split`、`string sub`、`string upper` 然后重新连接，代表了在 Fish Shell 中实现这一目标的惯用方法。
