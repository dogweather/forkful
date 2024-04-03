---
date: 2024-01-20 17:38:37.994295-07:00
description: "How to: (\u5982\u4F55\u505A\uFF1A) \u5728Fish Shell\u4E2D\uFF0C\u4F7F\
  \u7528`string`\u5DE5\u5177\uFF0C\u53EF\u4EE5\u8F7B\u677E\u5B9E\u73B0\u5B57\u7B26\
  \u4E32\u5C0F\u5199\u8F6C\u6362\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.248713-06:00'
model: gpt-4-1106-preview
summary: "\u5728Fish Shell\u4E2D\uFF0C\u4F7F\u7528`string`\u5DE5\u5177\uFF0C\u53EF\
  \u4EE5\u8F7B\u677E\u5B9E\u73B0\u5B57\u7B26\u4E32\u5C0F\u5199\u8F6C\u6362."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
