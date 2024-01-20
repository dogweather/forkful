---
title:                "检查目录是否存在"
html_title:           "Kotlin: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

检查一个目录是否存在是在程序中确认一个特定的文件目录是否真实存在的过程。程序员这么做主要是为了防止在对不存在的目录进行操作时候出现错误。

## 如何操作:

下面的Ruby代码可以用来检查一个目录是否存在：
```Ruby
if Dir.exist?("/path/to/directory")
  puts "Directory exists"
else
  puts "Directory does not exist"
end
```
如果目录存在，输出将会是 "Directory exists"，如果目录不存在，输出将会是 "Directory does not exist"。

## 深度解析

#### 历史背景

在早期的编程语言中，检查目录是否存在通常不是一项内置的功能，需要程序员自行实现。随着编程语言的发展，如Ruby等现代编程语言在库中都包含了这种功能，使得检查更为便捷。

#### 替代方法

除了使用Dir.exist?方法，你还能使用File.directory?方法来检查一个目录是否存在，方法如下：
```Ruby
if File.directory?("/path/to/directory")
  puts "Directory exists"
else
  puts "Directory does not exist"
end
```

#### 执行细节

在Ruby中, Dir.exist? 和 File.directory?这两个方法都直接调用底层的文件系统API，所以执行速度上几乎没有区别。

## 参见

你可以在Ruby官方文档中查看更多有关这两个方法的详细信息和更多示例： 
- [`Dir.exist?` documentation](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [`File.directory?` documentation](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)