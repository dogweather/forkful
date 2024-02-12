---
title:                "检查目录是否存在"
aliases:
- zh/bash/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:41.348699-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
