---
date: 2024-01-20 17:37:10.821213-07:00
description: "How to: (\u600E\u4E48\u505A) Lua\u7684`os.date`\u51FD\u6570\u6E90\u81EA\
  C\u8BED\u8A00\u7684`strftime`\u51FD\u6570\uFF0C\u7528\u4E8E\u65E5\u671F\u548C\u65F6\
  \u95F4\u7684\u683C\u5F0F\u5316\u3002\u5728Lua\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.132294-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A) Lua\u7684`os.date`\u51FD\u6570\u6E90\u81EAC\u8BED\u8A00\
  \u7684`strftime`\u51FD\u6570\uFF0C\u7528\u4E8E\u65E5\u671F\u548C\u65F6\u95F4\u7684\
  \u683C\u5F0F\u5316\u3002\u5728Lua 5.1\u4E4B\u540E\uFF0C`os.date`\u652F\u6301\u4EE5\
  UTC\u683C\u5F0F\u8FD4\u56DE\u65F6\u95F4\u3002\u9009\u62E9\u4F7F\u7528`os.date`\u7684\
  \u539F\u56E0\u662F\u56E0\u4E3A\u5B83\u7B80\u5355\u4E14\u8DE8\u5E73\u53F0\u3002Lua\u4E5F\
  \u6709\u5176\u4ED6\u7B2C\u4E09\u65B9\u5E93\u5982`luadate`\u63D0\u4F9B\u66F4\u591A\
  \u529F\u80FD\uFF0C\u4F46`os.date`\u901A\u5E38\u8DB3\u591F\u7528\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## How to: (怎么做)
在Lua中，使用`os.date`来转换日期。

```Lua
local current_time = os.time() -- 获取当前时间
local date_string = os.date("%Y-%m-%d %H:%M:%S", current_time) -- 转换为字符串

print(date_string) -- 打印字符串格式的日期

-- 输出示例:
-- 2023-04-07 15:26:31
```

## Deep Dive (深度解析)
Lua的`os.date`函数源自C语言的`strftime`函数，用于日期和时间的格式化。在Lua 5.1之后，`os.date`支持以UTC格式返回时间。选择使用`os.date`的原因是因为它简单且跨平台。Lua也有其他第三方库如`luadate`提供更多功能，但`os.date`通常足够用。

当需要处理不同时间格式或复杂日期逻辑时，上述库可能有所帮助。然而，`os.date`对大多数标准日期转换已经足够强大。

实现细节方面，`os.date`的转换模式遵循ISO 8601和当地的惯例。例如，`%Y`代表4位年份，`%m`是月份，`%d`是天数。这些模式符号提供了强大的灵活性。

## See Also (另请参阅)
- [Lua 5.4 参考手册: os.date](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- [GitHub: luadate - 一个第三方日期和时间操作库](https://github.com/Tieske/date)
