---
title:                "写入标准错误"
aliases:
- /zh/ruby/writing-to-standard-error/
date:                  2024-02-03T19:34:18.863606-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Ruby 中写入标准错误（stderr）是指将错误消息或诊断信息定向到与标准输出（stdout）不同的单独输出流。程序员这样做是为了区分常规程序输出与错误和调试信息，从而便于更容易地诊断问题和解析日志。

## 如何做：
Ruby 的标准库提供了一种简单的方式来使用 `$stderr` 或 `STDERR` 写入 stderr。对于这个基本操作，你不需要第三方库。

### 向 stderr 写入一条简单消息：
```ruby
$stderr.puts "Error: 文件未找到。"
# 或等效地
STDERR.puts "Error: 文件未找到。"
```
示例输出（到 stderr）：
```
Error: 文件未找到。
```

### 将 stderr 重定向到文件：
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "无法打开配置。"
end
```
这段代码将 stderr 重定向到名为 `error.log` 的文件中，所有随后写入的错误都会输出到那里，直到程序重置 stderr 重定向或终止。

### 在异常处理中使用 stderr：
```ruby
begin
  # 模拟可能失败的操作，例如，打开文件
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "发生异常：#{e.message}"
end
```
示例输出（到 stderr）：
```
发生异常：No such file or directory @ rb_sysopen - nonexistent_file.txt
```

虽然 Ruby 的内置方法写入 stderr 对于许多应用程序而言已经足够，但对于更复杂的日志需求，你可能会考虑使用 `logger` 标准库或外部宝石如 `Log4r`。这些提供了可配置的日志机制，包括严重性级别、格式化以及写入各种输出的能力，包括文件、电子邮件等。
