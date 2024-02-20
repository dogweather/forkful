---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:16.938325-07:00
description: "\u5728Ruby\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u5141\
  \u8BB8\u7A0B\u5E8F\u5458\u5728\u6267\u884C\u8BFB\u53D6\u6587\u4EF6\u6216\u521B\u5EFA\
  \u65B0\u76EE\u5F55\u7B49\u64CD\u4F5C\u4E4B\u524D\u9A8C\u8BC1\u76EE\u5F55\u7684\u5B58\
  \u5728\u3002\u8FD9\u5BF9\u4E8E\u907F\u514D\u6587\u4EF6\u5904\u7406\u9519\u8BEF\u548C\
  \u786E\u4FDD\u6587\u4EF6\u7CFB\u7EDF\u64CD\u4F5C\u7684\u53EF\u9760\u6027\u81F3\u5173\
  \u91CD\u8981\u3002"
lastmod: 2024-02-19 22:05:07.452448
model: gpt-4-0125-preview
summary: "\u5728Ruby\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u5141\u8BB8\
  \u7A0B\u5E8F\u5458\u5728\u6267\u884C\u8BFB\u53D6\u6587\u4EF6\u6216\u521B\u5EFA\u65B0\
  \u76EE\u5F55\u7B49\u64CD\u4F5C\u4E4B\u524D\u9A8C\u8BC1\u76EE\u5F55\u7684\u5B58\u5728\
  \u3002\u8FD9\u5BF9\u4E8E\u907F\u514D\u6587\u4EF6\u5904\u7406\u9519\u8BEF\u548C\u786E\
  \u4FDD\u6587\u4EF6\u7CFB\u7EDF\u64CD\u4F5C\u7684\u53EF\u9760\u6027\u81F3\u5173\u91CD\
  \u8981\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
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
