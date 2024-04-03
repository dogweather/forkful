---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:56.016482-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby\u4F7F\u5F97\u6587\u4EF6\u64CD\u4F5C\
  \u53D8\u5F97\u7B80\u5355\u76F4\u63A5\u3002\u8981\u5199\u5165\u6587\u4EF6\uFF0C\u60A8\
  \u53EF\u4EE5\u4F7F\u7528Ruby\u7684\u5185\u7F6E`File`\u7C7B\u3002\u4EE5\u4E0B\u793A\
  \u4F8B\u6F14\u793A\u4E86\u5982\u4F55\u6253\u5F00\u4E00\u4E2A\u6587\u4EF6\u4EE5\u8FDB\
  \u884C\u5199\u5165\uFF08`\"w\"`\u6A21\u5F0F\uFF09\u548C\u8FFD\u52A0\uFF08`\"a\"\
  `\u6A21\u5F0F\uFF09\uFF0C\u7136\u540E\u5411\u5176\u5199\u5165\u4E00\u4E2A\u5B57\u7B26\
  \u4E32\uFF0C\u5E76\u786E\u4FDD\u4E4B\u540E\u5173\u95ED\u6587\u4EF6\uFF1A."
lastmod: '2024-03-13T22:44:48.394220-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u4F7F\u5F97\u6587\u4EF6\u64CD\u4F5C\u53D8\u5F97\u7B80\u5355\u76F4\u63A5\
  \u3002\u8981\u5199\u5165\u6587\u4EF6\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528Ruby\u7684\
  \u5185\u7F6E`File`\u7C7B\u3002\u4EE5\u4E0B\u793A\u4F8B\u6F14\u793A\u4E86\u5982\u4F55\
  \u6253\u5F00\u4E00\u4E2A\u6587\u4EF6\u4EE5\u8FDB\u884C\u5199\u5165\uFF08`\"w\"`\u6A21\
  \u5F0F\uFF09\u548C\u8FFD\u52A0\uFF08`\"a\"`\u6A21\u5F0F\uFF09\uFF0C\u7136\u540E\u5411\
  \u5176\u5199\u5165\u4E00\u4E2A\u5B57\u7B26\u4E32\uFF0C\u5E76\u786E\u4FDD\u4E4B\u540E\
  \u5173\u95ED\u6587\u4EF6\uFF1A."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

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
