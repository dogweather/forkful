---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:54.603151-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua\u6CA1\u6709\u5185\u7F6E\u7684\u51FD\
  \u6570\u7528\u4E8E\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u4F46\u4F60\
  \u53EF\u4EE5\u4F7F\u7528\u57FA\u672C\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u51FD\u6570\
  \u8F7B\u677E\u5B8C\u6210\u6B64\u4EFB\u52A1\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\
  \u5355\u7684\u51FD\u6570\uFF0C\u7528\u4E8E\u5C06\u5355\u4E2A\u5355\u8BCD\u7684\u7B2C\
  \u4E00\u4E2A\u5B57\u6BCD\u5927\u5199\uFF1A."
lastmod: '2024-03-13T22:44:47.893344-06:00'
model: gpt-4-0125-preview
summary: "Lua\u6CA1\u6709\u5185\u7F6E\u7684\u51FD\u6570\u7528\u4E8E\u5B57\u7B26\u4E32\
  \u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u57FA\u672C\
  \u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u51FD\u6570\u8F7B\u677E\u5B8C\u6210\u6B64\u4EFB\
  \u52A1\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\u5355\u7684\u51FD\u6570\uFF0C\u7528\
  \u4E8E\u5C06\u5355\u4E2A\u5355\u8BCD\u7684\u7B2C\u4E00\u4E2A\u5B57\u6BCD\u5927\u5199\
  \uFF1A."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
Lua没有内置的函数用于字符串首字母大写，但你可以使用基本的字符串操作函数轻松完成此任务。这里有一个简单的函数，用于将单个单词的第一个字母大写：

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- 输出：Hello
```

要将句子中的每个单词都首字母大写，你可以将句子拆分成单词，将每个单词首字母大写，然后再拼接它们：

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- 输出：Hello World From Lua
```

如果你正在进行一个项目，其中性能至关重要，并且你发现自己需要更高级的字符串操作能力，考虑使用第三方库如`Penlight`。Penlight通过更多功能强大的字符串处理函数等实用程序增强了Lua：

```lua
-- 假设Penlight已安装：
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- 输出：Hello lua users

-- 注意：Penlight的capitalized函数只会使第一个单词首字母大写。
-- 要使每个单词都首字母大写，你仍需实现自定义解决方案或探索其他库。
```
