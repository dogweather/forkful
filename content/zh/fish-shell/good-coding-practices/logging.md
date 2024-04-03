---
date: 2024-01-26 01:03:48.598860-07:00
description: "\u65E5\u5FD7\u8BB0\u5F55\u672C\u8D28\u4E0A\u5C31\u662F\u8BB0\u5F55\u4E0B\
  \u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u5728\u505A\u4EC0\u4E48\u2014\u2014\u53EF\u4EE5\
  \u8BA4\u4E3A\u662F\u4EE3\u7801\u7684\u65E5\u8BB0\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u8DDF\u8E2A\u7EC6\u8282\uFF0C\u6BD4\u5982\u72B6\u6001\u53D8\
  \u5316\u3001\u7CFB\u7EDF\u4E8B\u4EF6\u548C\u96BE\u7F20\u7684\u9519\u8BEF\uFF0C\u786E\
  \u4FDD\u6CA1\u6709\u6545\u969C\u6084\u7136\u9003\u8FC7\u3002"
lastmod: '2024-03-13T22:44:48.273098-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u5FD7\u8BB0\u5F55\u672C\u8D28\u4E0A\u5C31\u662F\u8BB0\u5F55\u4E0B\
  \u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u5728\u505A\u4EC0\u4E48\u2014\u2014\u53EF\u4EE5\
  \u8BA4\u4E3A\u662F\u4EE3\u7801\u7684\u65E5\u8BB0\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u8DDF\u8E2A\u7EC6\u8282\uFF0C\u6BD4\u5982\u72B6\u6001\u53D8\
  \u5316\u3001\u7CFB\u7EDF\u4E8B\u4EF6\u548C\u96BE\u7F20\u7684\u9519\u8BEF\uFF0C\u786E\
  \u4FDD\u6CA1\u6709\u6545\u969C\u6084\u7136\u9003\u8FC7\u3002."
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

## 什么 & 为什么？
日志记录本质上就是记录下你的应用程序在做什么——可以认为是代码的日记。程序员这样做是为了跟踪细节，比如状态变化、系统事件和难缠的错误，确保没有故障悄然逃过。

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
