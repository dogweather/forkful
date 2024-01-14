---
title:    "Ruby: 读取文本文件"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

**<注解>**


## 为什么

阅读文本文件是编程中非常常见的技巧。通过阅读文本文件，您可以轻松地从外部资源中加载数据并将其应用到您的程序中。阅读本文了解如何从Ruby程序中读取文本文件吧！

## 如何做

阅读文本文件的过程非常简单。首先，您需要使用Ruby的内置`File`类来打开文件。使用`File.open`方法，您可以打开一个文本文件，并将其赋值给一个变量。然后，您可以使用`readlines`方法读取文件中的每一行，并将其存储在一个数组中，供后续使用。最后，记得使用`close`方法关闭文件。下面是一个示例代码，展示了如何读取文件中的内容并打印输出：

```Ruby
file = File.open("example.txt") # example.txt是你想要读取的文本文件
data = file.readlines # 将每一行存储在一个数组中
file.close # 关闭文件

# 使用循环遍历数组，并打印每一行的内容
data.each do |line|
  puts line
end
```

运行这段代码，您将会看到文件中的每一行被打印输出到控制台。

## 深入讨论

还有其他几种方法可以读取文本文件的内容。除了使用`File.open`和`readlines`方法外，您还可以使用`IO.readlines`方法来一次性读取文件的所有行，并返回一个数组。另外，您也可以使用`File.read`方法来将整个文本文件的内容读取为一个字符串。

除了读取文本文件之外，您还可以使用Ruby的`File`类来创建、写入和删除文件。通过查阅官方文档，您可以了解更多关于`File`类的用法和相关方法。

## 参考

[官方Ruby文档](https://ruby-doc.org/core-3.0.1/File.html)

[Ruby读取文本文件教程](https://www.rubyguides.com/2015/05/working-with-files-ruby/)

[Ruby文本文件处理教程](https://www.digitalocean.com/community/tutorials/how-to-use-file-io-functionality-in-ruby)

## 参见

[为什么和如何在Ruby中写入文本文件？](https://github.com/Gamma-Software/techblog/blob/master/ruby-writing-to-text-file.md)

[如何在Ruby中处理CSV文件？](https://github.com/Gamma-Software/techblog/blob/master/ruby-csv-processing.md)