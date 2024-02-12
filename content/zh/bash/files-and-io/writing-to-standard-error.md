---
title:                "写入标准错误"
aliases:
- /zh/bash/writing-to-standard-error/
date:                  2024-02-03T19:32:22.413835-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在 Bash 中写入标准错误 (stderr) 是关于将错误消息或任何重要的诊断输出与标准输出 (stdout) 分开指向。程序员这样做是为了确保错误消息可以被轻松识别、记录或甚至忽略，有助于调试和记录过程。

## 如何操作：
在 Bash 中，您使用 `>&2` 将输出重定向到 stderr。这是一个基本示例：

```bash
echo "这是一个普通消息"
echo "这是一个错误消息" >&2
```

运行这个脚本将在控制台显示两条消息，但如果您重定向它们，可以将 stdout 与 stderr 分开。例如：

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` 将包含 `"这是一个普通消息"`，而 `error.txt` 将捕获 `"这是一个错误消息"`。

对于一个实际的用例，考虑一个处理文件并在文件不存在时报告错误的脚本：

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename 不存在！" >&2
    exit 1
else
    echo "处理 $filename"
fi
```

当 `example.txt` 不存在时，直接在控制台上的样本输出：

```
example.txt 不存在！
```

Bash 中没有用于处理 stderr 的直接第三方库，因为重定向是原生支持的，并且通常足够用。然而，对于复杂的应用程序，可以结合使用日志框架或外部日志工具，如 `syslog` 或 `log4bash`，以更有效地管理 stdout 和 stderr。
