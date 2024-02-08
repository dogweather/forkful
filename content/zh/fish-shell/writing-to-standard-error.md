---
title:                "写入标准错误"
aliases:
- zh/fish-shell/writing-to-standard-error.md
date:                  2024-02-03T19:33:08.929601-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
