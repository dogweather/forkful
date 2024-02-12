---
title:                "编写文本文件"
aliases: - /zh/ruby/writing-a-text-file.md
date:                  2024-02-03T19:28:56.016482-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写文本文件"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
在Ruby中写入文本文件是一项基本操作，允许您持久地存储输出和数据，使得数据可以之后被访问或修改。程序员经常执行这项任务的原因包括记录日志、保存配置或以人类可读格式导出数据。

## 如何操作：
Ruby使得文件操作变得简单直接。要写入文件，您可以使用Ruby的内置`File`类。以下示例演示了如何打开一个文件以进行写入（`"w"`模式）和追加（`"a"`模式），然后向其写入一个字符串，并确保之后关闭文件：

```ruby
# 向文件写入新内容，覆盖现有内容
File.open("example.txt", "w") do |file|
  file.puts "Hello, Ruby!"
end

# 向文件末尾追加内容
File.open("example.txt", "a") do |file|
  file.puts "Adding another line."
end
```
执行以上两个代码片段后，`example.txt`的内容将是：
```
Hello, Ruby!
Adding another line.
```

### 使用第三方库：FileUtils
对于更复杂的文件操作，Ruby标准库`FileUtils`可能会派上用场，虽然对于基本的文件写入来说，标准的`File`方法已经足够。然而，如果您想复制、移动、删除或执行与文件写入相关的其他文件系统操作，探索`FileUtils`是值得的。

使用`FileUtils`创建目录然后在该目录中写入文件的示例：
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Log entry: #{Time.now}"
end
```

这展示了如果`logs`目录尚不存在，就创建一个新的目录`logs`，并在其中写入一个新文件`today.log`，展示了目录和文件操作，没有直接使用FileUtils进行写入，但利用了它的目录处理能力。
