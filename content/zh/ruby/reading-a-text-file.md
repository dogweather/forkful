---
title:                "Ruby: 读取文本文件"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

为什么：阅读文本文件的好处

阅读文本文件是程序员必备的技能之一。它可以帮助我们处理大量的文本数据，并从中提取有用的信息。无论是从网络抓取数据，还是从本地文件读取数据，掌握文本文件的读取技巧都是至关重要的。

如何进行文本文件读取：

```Ruby
# 打开文件
file = File.open("textfile.txt")

# 逐行读取文件，存储在数组中
lines = file.readlines

# 关闭文件
file.close

# 遍历数组，输出每一行文本
lines.each do |line|
    puts line
end
```

输出示例：
```
Hello world
This is a text file.
It contains multiple lines.
```

深入探讨：

除了逐行读取，我们还可以使用 Ruby 内置的 `File` 类的 `read` 方法来直接读取整个文件内容。例如：

```Ruby
# 打开文件
file = File.open("textfile.txt")

# 读取文件内容
content = file.read

# 关闭文件
file.close

# 输出文件内容
puts content
```

输出示例：
```
Hello world
This is a text file.
It contains multiple lines.
```

除了常规的文本文件，我们还可以使用 Ruby 的 `CSV` 模块来读取和处理 CSV 格式的文件。这可以大大简化我们的操作，同时也能提高程序的运行效率。关于 `CSV` 模块的更多详细信息，请参考官方文档。

## 参考链接：

- [Ruby 文档 - File 类](https://ruby-doc.org/core-2.7.2/File.html)
- [Ruby 文档 - CSV 模块](https://ruby-doc.org/stdlib-2.7.2/libdoc/csv/rdoc/CSV.html)
- [Ruby 教程 - 文件操作](https://ruby-china.org/wiki/file-operations-in-ruby)

请参考上述链接了解更多有关文本文件读取的知识。

## 参见：

- [如何使用 Ruby 写入文本文件？](https://example.com/article)
- [Ruby 中使用正则表达式提取文本信息](https://example.com/article)
- [探索 Ruby 中的文件操作：删除、重命名和复制文件](https://example.com/article)