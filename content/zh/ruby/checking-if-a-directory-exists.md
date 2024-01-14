---
title:                "Ruby: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，经常需要检查某个文件夹是否存在。这可以帮助我们确保我们的程序能够正常运行，并避免出现不必要的错误。我们可以使用Ruby来轻松检查一个文件夹是否存在，并根据需要采取相应的措施。

## 怎么做

首先，我们需要使用```Dir.exists?()```方法来检查文件夹是否存在。此方法将返回一个布尔值，如果文件夹存在，则为```true```，如果不存在，则为```false```。

```ruby
if Dir.exists?("documents")
  puts "Documents folder exists!"
else
  puts "Documents folder does not exist."
end
```

上述代码将根据文件夹的存在与否打印不同的信息。

另一种方法是使用```File.directory?()```方法来实现相同的功能。此方法也将返回一个布尔值，如果文件夹存在，则为```true```，如果不存在，则为```false```。

```ruby
if File.directory?("documents")
  puts "Documents folder exists!"
else
  puts "Documents folder does not exist."
end
```

无论是使用```Dir.exists?()```还是```File.directory?()```，我们都可以在需要时轻松确定一个文件夹是否存在。

## 深入探讨

在Ruby中，还有许多其他方法可以帮助我们检查文件夹的存在性。例如，我们可以使用```Dir.empty?()```方法来检查文件夹是否为空。此方法将返回一个布尔值，如果文件夹为空，则为```true```，如果不为空，则为```false```。

```ruby
if Dir.empty?("documents")
  puts "Documents folder is empty."
else
  puts "Documents folder contains files or subfolders."
end
```

我们还可以使用```Dir.glob()```方法来获取一个文件夹中所有文件或子文件夹的列表。此方法将返回一个数组，我们可以在代码中使用循环来访问每个文件或子文件夹。

```ruby
Dir.glob("documents/*").each do |item|
  puts item
end
```

## 参考链接

- [Dir.exists?() Ruby Doc](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exists-3F)
- [File.directory?() Ruby Doc](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)
- [Dir.empty?() Ruby Doc](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-empty-3F)
- [Dir.glob() Ruby Doc](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-glob)

## 参考链接