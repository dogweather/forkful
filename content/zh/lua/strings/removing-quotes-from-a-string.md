---
title:                "从字符串中移除引号"
aliases: - /zh/lua/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:35.068220-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
从字符串中剥去引号意味着将那些紧紧围绕你文本的双引号或单引号字符去除。编程人员这么做是为了清理输入、简化解析，或是为了统一可能引号使用不一致的数据。

## 如何操作：
以下是在Lua中将那些引号丢到一边的方法：

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hello, World!"'))     -- Hello, World!
print(remove_quotes("'Goodbye, Quotes!'"))  -- Goodbye, Quotes!
```

就这样！那些引号像烘干机里的袜子一样消失了。

## 深入探讨
自从语言能够处理文本以来，人们就一直在字符串中擦拭引号，这几乎是自古以来的事了。在Lua中，`gsub`函数承担起了重活，使用模式像手术刀一样切除引号。其他选择？当然，你可以在支持它的语言中使用正则表达式，或者自己编写一个遍历每个字符的循环（啊，可能会觉得无聊，但嘿，这是你的时间）。

Lua的模式匹配提供了类似正则的体验，无需导入整个库。脱字符(`^`)和美元符(`$`)分别匹配字符串的开始和结尾；`%p`匹配任何标点符号。在摆脱了前导和尾随的标点之后，我们用`(.*),`捕获其他所有内容，并使用`"%1"`替换整个匹配项，利用该捕获组。

请记住，Lua的模式匹配不如完全的正则引擎那么强大 — 例如，它不能计数或回溯。这种简单性既是福也是祸，这取决于你要处理的引号以及它们隐藏的位置。

## 另请参阅
更深入地了解Lua的模式匹配，可以参考PiL（Lua编程）书籍：http://www.lua.org/pil/20.2.html

为了纯粹的优雅，看看其他语言是如何做的进行比较，从Python的`str.strip`开始：https://docs.python.org/3/library/stdtypes.html#str.strip
