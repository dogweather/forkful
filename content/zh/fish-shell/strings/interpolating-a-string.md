---
date: 2024-01-20 17:50:59.362488-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u5C31\u662F\u5C06\u53D8\u91CF\u5185\u5BB9\
  \u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u52A8\u6001\u6784\u9020\u4FE1\u606F\uFF0C\u8BA9\u8F93\u51FA\u66F4\
  \u7075\u6D3B\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:07.302446
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u5C31\u662F\u5C06\u53D8\u91CF\u5185\u5BB9\
  \u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u52A8\u6001\u6784\u9020\u4FE1\u606F\uFF0C\u8BA9\u8F93\u51FA\u66F4\
  \u7075\u6D3B\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
字符串插值就是将变量内容嵌入到字符串中。程序员这样做是为了动态构造信息，让输出更灵活。

## How to: (怎么做)
Fish Shell 使用单引号`'`和双引号`"`来处理不同的字符串情况。变量展开（插值）需要使用双引号。

```Fish Shell
set name '小鱼'
echo "你好, $name!" # 使用 $ 进行变量插值
# 输出: 你好, 小鱼!

set count 3
echo "我有$count本书。" # 数字也可以插值
# 输出: 我有3本书。
```

## Deep Dive (深入探索)
Fish Shell 的字符串插值机制和其他Shell（如Bash）不同，它不需要像Bash那样用{}来明确变量边界，因为Fish更智能地解析变量。这一点为新用户提供了便利。但若需要在插值后紧跟文字而不留空格，确实需要用到`{}`。

比如：
```Fish Shell
set food '枣'
echo "我喜欢吃{$food}子"
# 输出: 我喜欢吃枣子
```

其他Shell，如Bash，通常需要更多的引号和大括号来处理复杂的插值。Fish Shell的设计理念是保持简单，所以它在这方面尽可能减少语法要求。

## See Also (另请参阅)
- 官方文档: [Fish Shell 文档](https://fishshell.com/docs/current/index.html)
- 社区问答：[Fish GitHub 讨论区](https://github.com/fish-shell/fish-shell/discussions)
