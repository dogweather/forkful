---
title:    "Ruby: 检查目录是否存在"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

经常在编写代码的过程中，我们需要判断某个目录是否存在。这往往是因为我们需要通过代码来操作文件或者文件夹，但是在操作之前，需要确保所需的目录是存在的。通过检查目录是否存在可以避免程序出现不必要的错误。

## 如何进行目录检查

在Ruby中，我们可以使用 `Dir.exist?` 方法来判断目录是否存在。这个方法会接收一个字符串作为参数，表示目录的路径，然后返回一个布尔值。如果目录存在，返回 `true`，否则返回 `false`。下面是一个简单的例子：

```Ruby
if Dir.exist?("my_directory")
  puts "目录存在"
else
  puts "目录不存在"
end
```

上面的代码中，我们先使用 `Dir.exist?` 方法判断 `my_directory` 目录是否存在，如果存在就打印出 "目录存在"，否则打印出 "目录不存在"。

## 深入了解目录检查

除了使用 `Dir.exist?` 方法，我们还可以使用 `File.directory?` 方法来进行目录检查。这个方法也接收一个字符串作为参数，表示目录的路径，返回的也是一个布尔值。下面是一个例子：

```Ruby
if File.directory?("other_directory")
  puts "目录存在"
else
  puts "目录不存在"
end
```

这两个方法都可以很方便地帮助我们判断目录是否存在，但是需要注意的是，它们只能检查路径中的目录是否存在，不能检查路径中的文件是否存在。

## 参考资料

- [Ruby文档: Dir模块](https://ruby-doc.org/core-2.6.3/Dir.html)
- [Ruby文档: File模块](https://ruby-doc.org/core-2.6.3/File.html)