---
date: 2024-01-26 03:40:35.068220-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u81EA\u4ECE\u8BED\u8A00\u80FD\u591F\u5904\
  \u7406\u6587\u672C\u4EE5\u6765\uFF0C\u4EBA\u4EEC\u5C31\u4E00\u76F4\u5728\u5B57\u7B26\
  \u4E32\u4E2D\u64E6\u62ED\u5F15\u53F7\uFF0C\u8FD9\u51E0\u4E4E\u662F\u81EA\u53E4\u4EE5\
  \u6765\u7684\u4E8B\u4E86\u3002\u5728Lua\u4E2D\uFF0C`gsub`\u51FD\u6570\u627F\u62C5\
  \u8D77\u4E86\u91CD\u6D3B\uFF0C\u4F7F\u7528\u6A21\u5F0F\u50CF\u624B\u672F\u5200\u4E00\
  \u6837\u5207\u9664\u5F15\u53F7\u3002\u5176\u4ED6\u9009\u62E9\uFF1F\u5F53\u7136\uFF0C\
  \u4F60\u53EF\u4EE5\u5728\u652F\u6301\u5B83\u7684\u8BED\u8A00\u4E2D\u4F7F\u7528\u6B63\
  \u5219\u8868\u8FBE\u5F0F\uFF0C\u6216\u8005\u81EA\u5DF1\u7F16\u5199\u4E00\u4E2A\u904D\
  \u5386\u6BCF\u4E2A\u5B57\u7B26\u7684\u5FAA\u73AF\uFF08\u554A\uFF0C\u53EF\u80FD\u4F1A\
  \u89C9\u5F97\u65E0\u804A\uFF0C\u4F46\u563F\uFF0C\u8FD9\u662F\u4F60\u7684\u65F6\u95F4\
  \uFF09\u3002\u2026"
lastmod: '2024-04-05T22:51:01.105772-06:00'
model: gpt-4-0125-preview
summary: "Lua\u7684\u6A21\u5F0F\u5339\u914D\u63D0\u4F9B\u4E86\u7C7B\u4F3C\u6B63\u5219\
  \u7684\u4F53\u9A8C\uFF0C\u65E0\u9700\u5BFC\u5165\u6574\u4E2A\u5E93\u3002\u8131\u5B57\
  \u7B26(`^`)\u548C\u7F8E\u5143\u7B26(`$`)\u5206\u522B\u5339\u914D\u5B57\u7B26\u4E32\
  \u7684\u5F00\u59CB\u548C\u7ED3\u5C3E\uFF1B`%p`\u5339\u914D\u4EFB\u4F55\u6807\u70B9\
  \u7B26\u53F7\u3002\u5728\u6446\u8131\u4E86\u524D\u5BFC\u548C\u5C3E\u968F\u7684\u6807\
  \u70B9\u4E4B\u540E\uFF0C\u6211\u4EEC\u7528`(.*),`\u6355\u83B7\u5176\u4ED6\u6240\u6709\
  \u5185\u5BB9\uFF0C\u5E76\u4F7F\u7528`\"%1\"`\u66FF\u6362\u6574\u4E2A\u5339\u914D\
  \u9879\uFF0C\u5229\u7528\u8BE5\u6355\u83B7\u7EC4\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

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
