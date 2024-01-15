---
title:                "检查目录是否存在"
html_title:           "Ruby: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么
有时候在编写代码时，我们需要检查一个目录是否存在。这个过程是为了避免出现错误，确保我们的程序正常运行。

## 如何
在Ruby中，我们可以使用`Dir.exist?()`方法来检查一个目录是否存在。它会返回一个布尔值，如果目录存在则为`true`，如果不存在则为`false`。

```Ruby
# 检查目录是否存在
puts Dir.exist?("documents") # true
puts Dir.exist?("pictures") # false
```

我们也可以使用`Dir.empty?()`方法来检查一个目录是否为空。它会返回一个布尔值，如果目录为空则为`true`，如果不为空则为`false`。

```Ruby
# 检查目录是否为空
puts Dir.empty?("documents") # false
puts Dir.empty?("pictures") # true
```

## 深入了解
如果我们想要进一步了解如何检查目录是否存在，我们可以了解更多关于`Dir`类的方法。除了`exist?()`和`empty?()`方法，还有其他有用的方法可以帮助我们处理目录。

一个例子是`Dir.chdir()`方法，它允许我们改变当前的工作目录。我们可以使用它来进入一个指定的目录并执行操作。

```Ruby
# 改变当前工作目录
Dir.chdir("documents")
puts Dir.exist?("documents") # true
```

## 参考链接
- [Ruby文档](https://ruby-doc.org/core-2.7.3/Dir.html) 
- [Dir类方法](https://www.rubyguides.com/2015/08/ruby-dir/)