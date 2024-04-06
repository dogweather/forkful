---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:16.938325-07:00
description: "\u5982\u4F55\u505A\uFF1A Ruby\u7684\u6807\u51C6\u5E93\u63D0\u4F9B\u4E86\
  \u76F4\u63A5\u7684\u65B9\u6CD5\u6765\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\
  \u3002\u4EE5\u4E0B\u662F\u4F7F\u7528\u7EAFRuby\u8FDB\u884C\u68C0\u67E5\u7684\u65B9\
  \u6CD5\uFF0C\u65E0\u9700\u4EFB\u4F55\u7B2C\u4E09\u65B9\u5E93\uFF1A."
lastmod: '2024-04-05T21:53:48.663009-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
