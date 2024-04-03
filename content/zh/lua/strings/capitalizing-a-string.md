---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:54.603151-07:00
description: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u4FEE\u6539\
  \u53E5\u5B50\u4E2D\u6BCF\u4E2A\u5355\u8BCD\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u4E3A\
  \u5927\u5199\u5B57\u6BCD\uFF0C\u540C\u65F6\u786E\u4FDD\u5176\u4F59\u5B57\u6BCD\u4E3A\
  \u5C0F\u5199\u3002\u8FD9\u79CD\u6280\u672F\u901A\u5E38\u7528\u4E8E\u683C\u5F0F\u5316\
  \u6587\u672C\uFF0C\u4EE5\u4FBF\u4EA7\u751F\u66F4\u4E13\u4E1A\u6216\u53EF\u8BFB\u7684\
  \u8F93\u51FA\uFF0C\u4F8B\u5982\u51C6\u5907\u6807\u9898\u6216\u7528\u6237\u8F93\u5165\
  \u4EE5\u4F9B\u663E\u793A\u3002"
lastmod: '2024-03-13T22:44:47.893344-06:00'
model: gpt-4-0125-preview
summary: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u4FEE\u6539\
  \u53E5\u5B50\u4E2D\u6BCF\u4E2A\u5355\u8BCD\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u4E3A\
  \u5927\u5199\u5B57\u6BCD\uFF0C\u540C\u65F6\u786E\u4FDD\u5176\u4F59\u5B57\u6BCD\u4E3A\
  \u5C0F\u5199\u3002\u8FD9\u79CD\u6280\u672F\u901A\u5E38\u7528\u4E8E\u683C\u5F0F\u5316\
  \u6587\u672C\uFF0C\u4EE5\u4FBF\u4EA7\u751F\u66F4\u4E13\u4E1A\u6216\u53EF\u8BFB\u7684\
  \u8F93\u51FA\uFF0C\u4F8B\u5982\u51C6\u5907\u6807\u9898\u6216\u7528\u6237\u8F93\u5165\
  \u4EE5\u4F9B\u663E\u793A\u3002."
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
