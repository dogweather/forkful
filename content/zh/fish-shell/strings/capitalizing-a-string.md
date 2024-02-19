---
aliases:
- /zh/fish-shell/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:30.126773-07:00
description: "\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u610F\u5473\
  \u7740\u4FEE\u6539\u5B83\u4F7F\u5F97\u7B2C\u4E00\u4E2A\u5B57\u6BCD\u4E3A\u5927\u5199\
  \uFF0C\u800C\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E3A\u5C0F\u5199\u3002\
  \u8FD9\u5728\u6587\u672C\u5904\u7406\u3001\u7528\u6237\u8F93\u5165\u6807\u51C6\u5316\
  \u4EE5\u53CA\u6570\u636E\u683C\u5F0F\u5316\u4E2D\u662F\u4E00\u9879\u5E38\u89C1\u7684\
  \u4EFB\u52A1\uFF0C\u7528\u4EE5\u786E\u4FDD\u4E00\u81F4\u6027\u6216\u6EE1\u8DB3\u7279\
  \u5B9A\u7684\u683C\u5F0F\u5316\u6807\u51C6\u3002"
lastmod: 2024-02-18 23:08:59.502465
model: gpt-4-0125-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u610F\u5473\
  \u7740\u4FEE\u6539\u5B83\u4F7F\u5F97\u7B2C\u4E00\u4E2A\u5B57\u6BCD\u4E3A\u5927\u5199\
  \uFF0C\u800C\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4E3A\u5C0F\u5199\u3002\
  \u8FD9\u5728\u6587\u672C\u5904\u7406\u3001\u7528\u6237\u8F93\u5165\u6807\u51C6\u5316\
  \u4EE5\u53CA\u6570\u636E\u683C\u5F0F\u5316\u4E2D\u662F\u4E00\u9879\u5E38\u89C1\u7684\
  \u4EFB\u52A1\uFF0C\u7528\u4EE5\u786E\u4FDD\u4E00\u81F4\u6027\u6216\u6EE1\u8DB3\u7279\
  \u5B9A\u7684\u683C\u5F0F\u5316\u6807\u51C6\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么与为何?

将字符串首字母大写，意味着修改它使得第一个字母为大写，而字符串的其余部分为小写。这在文本处理、用户输入标准化以及数据格式化中是一项常见的任务，用以确保一致性或满足特定的格式化标准。

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
