---
date: 2024-01-26 01:03:48.598860-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Fish \u4E2D\uFF0C\u8BB0\u5F55\u65E5\
  \u5FD7\u53EF\u4EE5\u50CF\u91CD\u5B9A\u5411\u6807\u51C6\u8F93\u51FA\u548C\u9519\u8BEF\
  \u6D41\u5230\u6587\u4EF6\u4E00\u6837\u7B80\u5355\u3002\u8BA9\u6211\u4EEC\u4E3A\u811A\
  \u672C\u7684\u5F00\u59CB\u548C\u7ED3\u675F\u65F6\u95F4\u5236\u4F5C\u4E00\u4E2A\u65E5\
  \u5FD7\u6761\u76EE\u3002"
lastmod: '2024-04-05T22:38:47.411047-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Fish \u4E2D\uFF0C\u8BB0\u5F55\u65E5\
  \u5FD7\u53EF\u4EE5\u50CF\u91CD\u5B9A\u5411\u6807\u51C6\u8F93\u51FA\u548C\u9519\u8BEF\
  \u6D41\u5230\u6587\u4EF6\u4E00\u6837\u7B80\u5355\u3002\u8BA9\u6211\u4EEC\u4E3A\u811A\
  \u672C\u7684\u5F00\u59CB\u548C\u7ED3\u675F\u65F6\u95F4\u5236\u4F5C\u4E00\u4E2A\u65E5\
  \u5FD7\u6761\u76EE\u3002"
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

## 如何操作：
在 Fish 中，记录日志可以像重定向标准输出和错误流到文件一样简单。让我们为脚本的开始和结束时间制作一个日志条目。

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - 脚本开始" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - 脚本结束" >> my_app.log
end

log_start
# ... 你脚本的任务 ...
log_end

cat my_app.log
```

在 `my_app.log` 中你会看到：

```
2023-04-01 10:35:47  - 脚本开始
2023-04-01 10:36:02  - 脚本结束
```

为了高级日志记录，你可以使用带参数的函数记录日志级别和信息：

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "这是一条信息性消息。"
log_message ERROR "出了些问题！"
```

`my_app.log` 的示例输出将会是：
```
2023-04-01 10:35:47 [INFO] 这是一条信息性消息。
2023-04-01 10:35:49 [ERROR] 出了些问题！
```

## 深入探讨
在历史上，在 shell 脚本中进行日志记录通常是通过许多 `echo` 语句完成的，而这个方法当然仍然是可行的，但实现更复杂的系统可能会是一个挑战。Fish 没有像其他一些 shell 或编程语言那样内置的日志记录机制，所以你通常需要自己创建。

Fish 的内置 `echo` 命令的替代方法包括 Unix 工具，如 `syslog` 或 `logger`，它们与系统日志守护进程接口，为记录系统范围内的事件提供了更集成的方法。

Fish 的简简单单使你能够创建函数来处理日志记录的详细程度，设置不同的级别，你可以开启或关闭。有些实现甚至可以包括脚本名称、行号和时间戳，这使得追溯到事件的过程变得更加容易。

## 参见
- Fish Shell 文档关于编写函数的部分：https://fishshell.com/docs/current/#syntax-function
- 基础 Shell 脚本编写技巧：https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Syslog 协议指南：https://tools.ietf.org/html/rfc5424
