---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:08.929601-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Fish Shell \u4E2D\uFF0C\u60A8\u53EF\
  \u4EE5\u901A\u8FC7\u4F7F\u7528 `>&2` \u91CD\u5B9A\u5411\u8F93\u51FA\u6765\u5199\u5165\
  \ stderr\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u4F8B\u5B50\uFF1A."
lastmod: '2024-04-05T22:38:47.423307-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Fish Shell \u4E2D\uFF0C\u60A8\u53EF\
  \u4EE5\u901A\u8FC7\u4F7F\u7528 `>&2` \u91CD\u5B9A\u5411\u8F93\u51FA\u6765\u5199\u5165\
  \ stderr\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u4F8B\u5B50\uFF1A."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 如何操作：
在 Fish Shell 中，您可以通过使用 `>&2` 重定向输出来写入 stderr。这里有一个基本例子：

```fish
echo "这是一条错误信息" >&2
```

此命令简单地将消息回显至 stderr 而非 stdout。如果您要编写一个同时输出常规消息和错误消息的脚本，您可能会这样做：

```fish
echo "开始处理"
echo "发生错误" >&2
echo "处理完成"
```

如果您运行脚本并将 stderr 重定向到文件的示例输出：

```
开始处理
处理完成
```

错误消息不会出现在标准输出中，但会出现在您重定向 stderr 的文件中。

在需要更复杂的错误处理或日志记录的场景中，Fish 没有内置专门为此设计的库。然而，您可以利用外部工具或编写函数来协助。例如，创建一个简单的日志记录函数可能如下所示：

```fish
function log_error
    echo $argv >&2
end

log_error "这是一条高级错误信息"
```

这个函数 `log_error` 将接受您给出的任何字符串并将其写入 stderr。使用此类函数可以帮助保持您的错误处理在脚本中清晰且一致。
