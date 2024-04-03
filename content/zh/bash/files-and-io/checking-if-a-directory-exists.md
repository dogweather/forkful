---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:41.348699-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Bash \u6838\u5FC3\u662F\u4F7F\u7528\u6761\
  \u4EF6\u8BED\u53E5\u548C `-d` \u64CD\u4F5C\u7B26\u6765\u68C0\u67E5\u76EE\u5F55\u662F\
  \u5426\u5B58\u5728\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u76F4\u63A5\u7684\u793A\u4F8B\
  \uFF0C\u5C55\u793A\u4E86\u5982\u4F55\u8FDB\u884C\u8FD9\u79CD\u68C0\u67E5\u3002"
lastmod: '2024-03-13T22:44:47.979106-06:00'
model: gpt-4-0125-preview
summary: "Bash \u6838\u5FC3\u662F\u4F7F\u7528\u6761\u4EF6\u8BED\u53E5\u548C `-d` \u64CD\
  \u4F5C\u7B26\u6765\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\u4E0B\u9762\
  \u662F\u4E00\u4E2A\u76F4\u63A5\u7684\u793A\u4F8B\uFF0C\u5C55\u793A\u4E86\u5982\u4F55\
  \u8FDB\u884C\u8FD9\u79CD\u68C0\u67E5."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
