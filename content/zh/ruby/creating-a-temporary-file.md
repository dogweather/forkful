---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？

创建临时文件是用程序生成且不需要永久保存的文件。 编程中，我们创作它们做临时存储，从而实现数据的中转，避免频繁的硬盘I/O操作，提高效率。

## 如何操作：

Ruby有一个内置的临时文件机制。以下是创建临时文件的例子：

```Ruby
require 'tempfile'

file = Tempfile.new('temp')
puts file.path     # 打印文件路径
file.puts("Hello World!")
file.close
```

运行上述代码，你会创建一个临时文件并写入"Hello World!"字符串。 `file.path` 将显示该文件存储的临时位置。

## 深入探讨：

**历史背景**：在早期的编程中，临时文件的创建和操作需要开发者完全自己完成，但随着Ruby内置临时文件机制的出现，大大简化了这个过程。

**备选方案**：除了 `Tempfile.new`，还可以使用 `Tempfile.create` 创建临时文件：

```Ruby
Tempfile.create('temp') do |file|
  file.puts("Hello again, World!")
end
```

这种做法的优点是在代码块结束时，临时文件会被自动关闭和删除。

**实现细节**：在内部，`Tempfile`类创建文件时会随机生成一个唯一的文件名，这样每次生成的临时文件都是唯一的。临时文件默认存放在系统的临时目录下。

## 更多参考：

[Ruby官方文档](https://ruby-doc.org/stdlib-3.0.0/libdoc/tempfile/rdoc/Tempfile.html)

[关于临时文件](https://en.wikipedia.org/wiki/Temporary_folder)

继续探索 Ruby，您将发现更多有用功能可用于日常编程。 同样，临时文件也是其中之一。