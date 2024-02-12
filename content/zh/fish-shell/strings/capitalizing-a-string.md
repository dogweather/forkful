---
title:                "字符串大写化"
aliases:
- /zh/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:05:30.126773-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
