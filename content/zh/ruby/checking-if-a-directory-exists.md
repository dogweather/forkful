---
title:                "Ruby: 检查目录是否存在"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

```
## 为什么要检查文件夹是否存在
文件处理是编程中常见的任务。有时，需要查找某个特定的文件夹，但在执行任何操作之前，我们需要先检查该文件夹是否存在。这样可以避免程序因为找不到文件夹而发生错误。

需要检查文件夹是否存在的情况可能很多，有时可能是因为用户输入的路径不正确，有时可能是因为程序需要使用特定的文件夹来存储数据。不管是什么原因，检查文件夹是否存在都可以帮助我们在编程中更加有效地处理文件。

## 如何检查文件夹是否存在
在Ruby中，我们可以使用`Dir.exist?()`方法来检查文件夹是否存在。下面是一个简单的例子：

```Ruby
if Dir.exist?("/Users/username/Desktop")
  puts "文件夹存在"
else
  puts "文件夹不存在"
end
```

上面的代码会遍历当前用户桌面上的文件夹，如果存在名为“username”的文件夹，则会输出“文件夹存在”，否则会输出“文件夹不存在”。

我们也可以使用绝对路径或相对路径来检查特定的文件夹，如：

```Ruby
if Dir.exist?("Documents/Files")
  puts "文件夹存在"
else
  puts "文件夹不存在"
end
```

## 深入了解检查文件夹是否存在
`Dir.exist?()`方法检查的是路径是否指向一个存在的文件夹，如果路径指向一个文件，则会返回`false`。如果需要检查路径是否指向一个文件夹或文件，可以使用`File.exist?()`方法。

除了`Dir.exist?()`和`File.exist?()`方法，Ruby中还有一些其他方法可以用来检查文件夹是否存在，如`File.directory?()`和`FileTest.directory?()`。这些方法的使用方式与`Dir.exist?()`类似，只是返回的结果可能稍有不同。

## 参考资料
- [Ruby核心文档-Dir](https://ruby-doc.org/core-2.7.0/Dir.html)
- [掌握文件和文件夹的操作](https://www.jianshu.com/p/aae55f1318e5)
- [Ruby中的文件和文件夹操作](https://www.runoob.com/ruby/ruby-files-io.html)

### 查看也可以
[如何创建和删除文件夹](https://example.com/create-delete-directory)
[遍历文件夹中的所有文件](https://example.com/traverse-directory)
[处理文件路径中的特殊字符](https://example.com/handle-special-characters)
```