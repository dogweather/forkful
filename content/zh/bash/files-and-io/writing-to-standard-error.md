---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:22.413835-07:00
description: "\u5728 Bash \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF (stderr) \u662F\
  \u5173\u4E8E\u5C06\u9519\u8BEF\u6D88\u606F\u6216\u4EFB\u4F55\u91CD\u8981\u7684\u8BCA\
  \u65AD\u8F93\u51FA\u4E0E\u6807\u51C6\u8F93\u51FA (stdout) \u5206\u5F00\u6307\u5411\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u9519\u8BEF\
  \u6D88\u606F\u53EF\u4EE5\u88AB\u8F7B\u677E\u8BC6\u522B\u3001\u8BB0\u5F55\u6216\u751A\
  \u81F3\u5FFD\u7565\uFF0C\u6709\u52A9\u4E8E\u8C03\u8BD5\u548C\u8BB0\u5F55\u8FC7\u7A0B\
  \u3002"
lastmod: '2024-03-11T00:14:21.778166-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Bash \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF (stderr) \u662F\u5173\
  \u4E8E\u5C06\u9519\u8BEF\u6D88\u606F\u6216\u4EFB\u4F55\u91CD\u8981\u7684\u8BCA\u65AD\
  \u8F93\u51FA\u4E0E\u6807\u51C6\u8F93\u51FA (stdout) \u5206\u5F00\u6307\u5411\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u9519\u8BEF\u6D88\
  \u606F\u53EF\u4EE5\u88AB\u8F7B\u677E\u8BC6\u522B\u3001\u8BB0\u5F55\u6216\u751A\u81F3\
  \u5FFD\u7565\uFF0C\u6709\u52A9\u4E8E\u8C03\u8BD5\u548C\u8BB0\u5F55\u8FC7\u7A0B\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
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
