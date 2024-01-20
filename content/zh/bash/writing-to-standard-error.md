---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
写入标准错误即将错误信息发往特定的输出流（stderr），而不是常规的标准输出（stdout）。程序员这么做是为了便于诊断错误，并将日志信息与正常输出区分开来。

## 怎么做：
```Bash
#!/bin/bash

# 正确信息输出到 stdout
echo "这是一条正常信息"

# 错误信息输出到 stderr
echo "这是一条错误信息" >&2
```

输出样例：
```
这是一条正常信息
这是一条错误信息
```

## 深入探索：
`stderr`起源于早期的 Unix 系统，是程序设计的标准部分，旨在处理错误输出。除了直接写入stderr之外，还可以重定向错误日志到文件、其他程序或远端服务中。在 Bash 中，`2>` 能够将标准错误重定向到文件中。例如，`command 2> error.log`会将错误信息输出到`error.log`文件。实现时，`stderr`通常用文件描述符 2 表达，而`stdout`是文件描述符 1。

## 参见链接：
- Bash 手册关于重定向的部分: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections
- Advanced Bash-Scripting Guide的错误处理部分: https://tldp.org/LDP/abs/html/io-redirection.html