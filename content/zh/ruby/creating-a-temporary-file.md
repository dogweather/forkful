---
title:    "Ruby: 创建临时文件"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么要创建临时文件？

有时候，当我们在编写Ruby程序时，需要将一些数据暂时保存到文件中。这时候，创建临时文件就非常有用了。它能让我们暂时保存数据，并保证不会影响到我们的原始数据。

如何做？

```Ruby
# 创建一个临时文件
temp_file = Tempfile.new("example")

# 写入数据
temp_file.write("这是临时文件中的数据！")

# 将数据读取出来
temp_file.rewind
puts temp_file.read

# 关闭文件
temp_file.close
```

输出：

```
这是临时文件中的数据！
```

深入探讨：

创建临时文件的过程实际上涉及到了操作系统的底层原理。在Ruby中，我们使用Tempfile类来创建临时文件。这个类会在操作系统中创建一个真正的临时文件，并提供一系列方法来操作这个临时文件。最后，我们需要记得关闭这个临时文件，否则会在系统中留下无用的临时文件。

参考链接：

1. [Ruby文档：Tempfile](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
2. [为什么要使用临时文件？](https://stackoverflow.com/questions/3419761/why-do-we-need-temporary-files)
3. [临时文件使用注意事项](https://www.prozilla.net/tech/ruby-programming-tips-for-temporary-file/)

## 参考链接

1. [为什么要使用临时文件？](https://stackoverflow.com/questions/3419761/why-do-we-need-temporary-files)
2. [临时文件使用注意事项](https://www.prozilla.net/tech/ruby-programming-tips-for-temporary-file/)