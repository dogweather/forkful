---
aliases:
- /zh/bash/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:41.348699-07:00
description: "\u5728 Bash \u7F16\u7A0B\u4E2D\uFF0C\u68C0\u67E5\u76EE\u5F55\u662F\u5426\
  \u5B58\u5728\u662F\u4E00\u79CD\u57FA\u672C\u7684\u63A7\u5236\u673A\u5236\uFF0C\u7528\
  \u4E8E\u5728\u6267\u884C\u6587\u4EF6\u64CD\u4F5C\u4E4B\u524D\u9A8C\u8BC1\u76EE\u5F55\
  \u7684\u5B58\u5728\u6027\u3002\u8FD9\u79CD\u68C0\u67E5\u5BF9\u4E8E\u907F\u514D\u5C1D\
  \u8BD5\u8BBF\u95EE\u6216\u4FEE\u6539\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u7684\u9519\
  \u8BEF\u81F3\u5173\u91CD\u8981\uFF0C\u4FDD\u8BC1\u811A\u672C\u6267\u884C\u66F4\u52A0\
  \u987A\u6ED1\u548C\u53EF\u9884\u6D4B\u3002"
lastmod: 2024-02-18 23:08:59.304976
model: gpt-4-0125-preview
summary: "\u5728 Bash \u7F16\u7A0B\u4E2D\uFF0C\u68C0\u67E5\u76EE\u5F55\u662F\u5426\
  \u5B58\u5728\u662F\u4E00\u79CD\u57FA\u672C\u7684\u63A7\u5236\u673A\u5236\uFF0C\u7528\
  \u4E8E\u5728\u6267\u884C\u6587\u4EF6\u64CD\u4F5C\u4E4B\u524D\u9A8C\u8BC1\u76EE\u5F55\
  \u7684\u5B58\u5728\u6027\u3002\u8FD9\u79CD\u68C0\u67E5\u5BF9\u4E8E\u907F\u514D\u5C1D\
  \u8BD5\u8BBF\u95EE\u6216\u4FEE\u6539\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u7684\u9519\
  \u8BEF\u81F3\u5173\u91CD\u8981\uFF0C\u4FDD\u8BC1\u811A\u672C\u6267\u884C\u66F4\u52A0\
  \u987A\u6ED1\u548C\u53EF\u9884\u6D4B\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
---

{{< edit_this_page >}}

## 什么与为什么？

在 Bash 编程中，检查目录是否存在是一种基本的控制机制，用于在执行文件操作之前验证目录的存在性。这种检查对于避免尝试访问或修改不存在的目录的错误至关重要，保证脚本执行更加顺滑和可预测。

## 如何操作：

Bash 核心是使用条件语句和 `-d` 操作符来检查目录是否存在。下面是一个直接的示例，展示了如何进行这种检查。

```bash
if [ -d "/path/to/directory" ]; then
    echo "目录存在。"
else
    echo "目录不存在。"
fi
```

样例输出（如果目录存在）:
```
目录存在。
```

样例输出（如果目录不存在）:
```
目录不存在。
```

对于更复杂的脚本，常见的做法是将检查与其他操作结合，比如如果目录不存在，则创建它：

```bash
DIR="/path/to/directory"
if [ -d "$DIR" ]; then
    echo "$DIR 存在。"
else
    echo "$DIR 不存在。正在创建..."
    mkdir -p "$DIR"
    echo "$DIR 已创建。"
fi
```

样例输出（如果目录不存在然后被创建）:
```
/path/to/directory 不存在。正在创建...
/path/to/directory 已创建。
```

尽管 Bash 本身提供了强大的工具进行此类检查，但没有专门用于此任务的流行第三方库，因为原生 Bash 命令已完全能够和高效地完成目录存在性验证。
