---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:18.863606-07:00
description: "\u5982\u4F55\u505A\uFF1A Ruby \u7684\u6807\u51C6\u5E93\u63D0\u4F9B\u4E86\
  \u4E00\u79CD\u7B80\u5355\u7684\u65B9\u5F0F\u6765\u4F7F\u7528 `$stderr` \u6216 `STDERR`\
  \ \u5199\u5165 stderr\u3002\u5BF9\u4E8E\u8FD9\u4E2A\u57FA\u672C\u64CD\u4F5C\uFF0C\
  \u4F60\u4E0D\u9700\u8981\u7B2C\u4E09\u65B9\u5E93\u3002"
lastmod: '2024-04-05T22:38:47.526453-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u505A\uFF1A Ruby \u7684\u6807\u51C6\u5E93\u63D0\u4F9B\u4E86\
  \u4E00\u79CD\u7B80\u5355\u7684\u65B9\u5F0F\u6765\u4F7F\u7528 `$stderr` \u6216 `STDERR`\
  \ \u5199\u5165 stderr\u3002\u5BF9\u4E8E\u8FD9\u4E2A\u57FA\u672C\u64CD\u4F5C\uFF0C\
  \u4F60\u4E0D\u9700\u8981\u7B2C\u4E09\u65B9\u5E93\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
