---
title:                "检查目录是否存在"
aliases:
- /zh/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:16.938325-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？
在Ruby中检查目录是否存在允许程序员在执行读取文件或创建新目录等操作之前验证目录的存在。这对于避免文件处理错误和确保文件系统操作的可靠性至关重要。

## 如何做：
Ruby的标准库提供了直接的方法来检查目录是否存在。以下是使用纯Ruby进行检查的方法，无需任何第三方库：

```ruby
require 'fileutils'

# 检查目录是否存在
if Dir.exist?('/path/to/directory')
  puts '目录存在。'
else
  puts '目录不存在。'
end
```
示例输出：
```
目录存在。
```
或：
```
目录不存在。
```

除了使用`Dir.exist?`之外，你还可以使用`File.directory?`方法，如果给定的路径是一个目录，它会返回`true`：

```ruby
if File.directory?('/path/to/directory')
  puts '目录存在。'
else
  puts '目录不存在。'
end
```
`Dir.exist?`和`File.directory?`都是Ruby标准库的一部分，使用它们不需要任何外部gems，这使它们成为检查目录的便捷且高效的选项。
