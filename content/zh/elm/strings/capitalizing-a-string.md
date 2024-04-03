---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:18.558406-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Elm\u4E2D\uFF0C\u6CA1\u6709\u4E00\
  \u4E2A\u5185\u7F6E\u51FD\u6570\u662F\u4E13\u95E8\u7528\u6765\u9996\u5B57\u6BCD\u5927\
  \u5199\u5B57\u7B26\u4E32\u7684\u3002\u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\u901A\u8FC7\
  \u4F7F\u7528\u5185\u7F6E\u7684`String`\u6A21\u5757\u51FD\u6570\uFF0C\u5982`toUpper`\u3001\
  `toLower`\u3001`left`\u548C`dropLeft`\uFF0C\u6765\u8F7B\u677E\u5B9E\u73B0\u8FD9\u4E00\
  \u70B9\u3002"
lastmod: '2024-03-13T22:44:47.654055-06:00'
model: gpt-4-0125-preview
summary: "\u5728Elm\u4E2D\uFF0C\u6CA1\u6709\u4E00\u4E2A\u5185\u7F6E\u51FD\u6570\u662F\
  \u4E13\u95E8\u7528\u6765\u9996\u5B57\u6BCD\u5927\u5199\u5B57\u7B26\u4E32\u7684\u3002\
  \u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528\u5185\u7F6E\u7684`String`\u6A21\
  \u5757\u51FD\u6570\uFF0C\u5982`toUpper`\u3001`toLower`\u3001`left`\u548C`dropLeft`\uFF0C\
  \u6765\u8F7B\u677E\u5B9E\u73B0\u8FD9\u4E00\u70B9."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
在Elm中，没有一个内置函数是专门用来首字母大写字符串的。然而，你可以通过使用内置的`String`模块函数，如`toUpper`、`toLower`、`left`和`dropLeft`，来轻松实现这一点。

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- 示例用法
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- 输出："Hello World"
```

对于更复杂的场景，或者如果你更愿意使用一个提供直接首字母大写字符串的库，你可能会考虑使用第三方包，比如`elm-community/string-extra`。然而，根据我最后的更新，Elm的生态系统鼓励使用内置函数来处理此类任务，以保持语言和项目的精简。

```elm
import String.Extra as StringExtra

-- 如果第三方库中有`capitalize`函数的情况
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- 使用假想库函数的示例用法
main =
    "this is elm" |> capitalizeWithLibrary
    -- 假想输出："This is elm"
```

如果你正在寻找额外的功能，超出标准库的范畴，请始终检查Elm包仓库，了解最新和最受欢迎的字符串操作库。
