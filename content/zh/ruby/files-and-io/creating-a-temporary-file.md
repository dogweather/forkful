---
date: 2024-01-20 17:41:06.260220-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u6307\u751F\u6210\u4E00\u4E2A\
  \u77ED\u6682\u5B58\u5728\u7684\u6587\u4EF6\uFF0C\u7A0B\u5E8F\u6267\u884C\u5B8C\u6BD5\
  \u540E\u901A\u5E38\u4F1A\u81EA\u52A8\u5220\u9664\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u5B83\u4EEC\u6765\u5B58\u50A8\u4E34\u65F6\u6570\u636E\uFF0C\u6216\u662F\u5728\u4E0D\
  \u5F71\u54CD\u539F\u59CB\u6570\u636E\u7684\u60C5\u51B5\u4E0B\u6D4B\u8BD5\u4EE3\u7801\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.395359-06:00'
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u6307\u751F\u6210\u4E00\u4E2A\
  \u77ED\u6682\u5B58\u5728\u7684\u6587\u4EF6\uFF0C\u7A0B\u5E8F\u6267\u884C\u5B8C\u6BD5\
  \u540E\u901A\u5E38\u4F1A\u81EA\u52A8\u5220\u9664\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u5B83\u4EEC\u6765\u5B58\u50A8\u4E34\u65F6\u6570\u636E\uFF0C\u6216\u662F\u5728\u4E0D\
  \u5F71\u54CD\u539F\u59CB\u6570\u636E\u7684\u60C5\u51B5\u4E0B\u6D4B\u8BD5\u4EE3\u7801\
  \u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? / 什么以及为什么？
创建临时文件是指生成一个短暂存在的文件，程序执行完毕后通常会自动删除。程序员使用它们来存储临时数据，或是在不影响原始数据的情况下测试代码。

## How to: / 如何操作：
使用 Ruby 创建临时文件，可以使用 `Tempfile` 类。实际示例如下：

```Ruby
require 'tempfile'

# 创建一个临时文件
Tempfile.create('tempfile_demo') do |tempfile|
  # 输出临时文件路径
  puts tempfile.path
  # 向临时文件写入内容
  tempfile.write('Hello, Mandarin Readers!')
  # 确保内容写入
  tempfile.flush
  # 读取内容展示
  puts File.read(tempfile.path)
end

# 注意: 代码块结束时，临时文件会被自动删除
```

当运行上面的代码时，你会看到输出了临时文件的路径和写入的内容，但最终这个文件会被删除，不会留在磁盘上。

## Deep Dive / 深入探讨：
在历史上，程序员手动管理临时文件时会面临文件名冲突和必须清理这些文件等问题。`Tempfile` 类于 Ruby 1.9 引入，它基于 `Dir::mktmpdir` 和 `File` 类构建，提供安全创建和使用临时文件的方法。

除了使用 `Tempfile`，Ruby 还提供了其他一些方式来操作文件和目录，比如 `FileUtils` 和 `File` 类。但是，`Tempfile` 是处理临时文件最简单、最安全的做法。

当创建临时文件时，`Tempfile` 会在一个特定的临时目录中创建文件，并确保唯一的文件名，避免名称冲突。默认情况下，临时文件是在 `Dir.tmpdir` 指定的目录下创建的，但可以修改临时目录的位置。

临时文件也与垃圾回收机制(GC)相关联，当没有任何引用指向这个文件对象时，GC会标记它进行清除。这使得程序内存管理更为高效。

## See Also / 相关资源：
- Ruby `File` 类文档：[Ruby-Doc File](https://ruby-doc.org/core/File.html)
