---
title:                "检查目录是否存在"
date:                  2024-01-20T14:58:16.705651-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
检查目录是否存在就是确认某个文件夹在电脑上是否真的存在。程序员这么做是为了避免错误，例如尝试访问不存在的目录会导致程序崩溃。

## How to: (如何操作：)
在Ruby中，你可以使用`Dir.exist?`方法检查目录是否存在。看看下面的例子：

```Ruby
if Dir.exist?('/path/to/directory')
  puts 'Directory exists!'
else
  puts 'Directory does not exist.'
end
```

如果目录存在，输出将是：

```
Directory exists!
```

如果不存在，输出将是：

```
Directory does not exist.
```

## Deep Dive (深入了解)
在Ruby的早期版本中，你可能会使用`File.exist?`或`File.directory?`来检查目录是否存在。`Dir.exist?`是在Ruby 1.9中引入的，现在是最推荐的用法，因为它更清晰地表达了你的意图。值得一提的是，文件和目录实际上都被认为是文件系统中的“文件”，所以旧的方法仍然有效，但`Dir.exist?`是专门用来检查目录的。

其它语言中，检查目录存在的方法可能不同。例如，在Python中，你会用`os.path.isdir`方法。

在实际项目中，检查目录是否存在通常是文件操作前的初步步骤，比如说读取或写入文件时。

## See Also (另请参阅)
- Ruby官方文档关于`Dir.exist?`的页面: [Dir.exist?](https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exist-3F)
- Stack Overflow上关于检查目录是否存在的讨论: [How to check if a directory exists in Ruby](https://stackoverflow.com/questions/5471032/how-to-check-if-a-directory-exists-in-ruby)
