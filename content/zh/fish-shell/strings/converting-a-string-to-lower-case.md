---
title:                "将字符串转换为小写"
aliases:
- zh/fish-shell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:37.994295-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
字符串转换为小写就是将所有文字字符改为小写形式。程序员这么做通常是为了统一数据格式，避免在比较或处理文本时大小写差异带来的问题。

## How to: (如何做：)
在Fish Shell中，使用`string`工具，可以轻松实现字符串小写转换。

```Fish Shell
echo "HELLo WorLD!" | string lower
```

输出结果：

```
hello world!
```

## Deep Dive (深入探讨)
Fish Shell的`string`命令是一个多功能工具，自从2.3.0版本引入以来，它就包括了转换大小写的功能。历史上，你可能需要依赖`awk`，`tr`等工具来完成这样的任务。与这些传统工具不同，`string`更为现代，简化了字符串操作。

### 替代方案：
你也可以使用如下工具来实现相同的目标：

- `awk '{print tolower($0)}'`
- `tr`命令的使用例子：`echo "HELLO WORLD!" | tr '[:upper:]' '[:lower:]'`

### 实现细节：
Fish中的`string lower`命令是用C++编写的，直接编译到Fish Shell内部。这导致它比起调用外部程序来说，执行转换的速度更快，效率更高。

## See Also (参考链接)
- Fish Shell官方文档：[string](https://fishshell.com/docs/current/cmds/string.html)
- Fish Shell GitHub 仓库：[FishShell](https://github.com/fish-shell/fish-shell)
