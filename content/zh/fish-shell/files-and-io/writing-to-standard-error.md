---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:08.929601-07:00
description: "\u5728 Fish Shell \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u662F\u5173\u4E8E\u5C06\u9519\u8BEF\u4FE1\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u4E0E\
  \u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\u6307\u5BFC\u7684\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u53EF\u4EE5\u8F7B\u677E\
  \u8BC6\u522B\u3001\u7BA1\u7406\u6216\u91CD\u5B9A\u5411\u9519\u8BEF\u4FE1\u606F\uFF0C\
  \u4ECE\u800C\u4FC3\u8FDB\u8C03\u8BD5\u548C\u65E5\u5FD7\u8BB0\u5F55\u8FC7\u7A0B\u7684\
  \u987A\u5229\u8FDB\u884C\u3002"
lastmod: '2024-03-13T22:44:48.284076-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Fish Shell \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u662F\u5173\u4E8E\u5C06\u9519\u8BEF\u4FE1\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u4E0E\
  \u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\u6307\u5BFC\u7684\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u53EF\u4EE5\u8F7B\u677E\
  \u8BC6\u522B\u3001\u7BA1\u7406\u6216\u91CD\u5B9A\u5411\u9519\u8BEF\u4FE1\u606F\uFF0C\
  \u4ECE\u800C\u4FC3\u8FDB\u8C03\u8BD5\u548C\u65E5\u5FD7\u8BB0\u5F55\u8FC7\u7A0B\u7684\
  \u987A\u5229\u8FDB\u884C\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 什么 & 为什么？

在 Fish Shell 中写入标准错误（stderr）是关于将错误信息或诊断信息与标准输出（stdout）分开指导的。程序员这样做是为了确保可以轻松识别、管理或重定向错误信息，从而促进调试和日志记录过程的顺利进行。

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
