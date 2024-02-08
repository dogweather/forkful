---
title:                "字符串大写化"
aliases:
- zh/lua/capitalizing-a-string.md
date:                  2024-02-03T19:05:54.603151-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
字符串首字母大写涉及修改句子中每个单词的第一个字符为大写字母，同时确保其余字母为小写。这种技术通常用于格式化文本，以便产生更专业或可读的输出，例如准备标题或用户输入以供显示。

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
