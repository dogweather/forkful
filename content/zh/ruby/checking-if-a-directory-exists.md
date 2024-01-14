---
title:    "Ruby: 检查目录是否存在"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

为什么：只有1-2句话来解释为什么某人会去检查文件夹是否存在。

Ruby编程简介：
如果你是一个正在学习Ruby编程语言的初学者，那么你可能会经常在你的代码中遇到需要检查某个目录是否存在的情况。这可能是因为你需要在特定的目录下创建文件，或者需要在某个目录下查找特定的文件。无论是出于何种原因，检查目录是否存在是很重要的一步，它可以确保你的代码能够顺利执行。

怎样实现：
在Ruby中，你可以通过使用Dir.exist? 方法来检查某个目录是否存在。这个方法会以布尔值的形式返回结果，如果目录存在则返回true，反之则返回false。下面是一个简单的例子：

```Ruby
if Dir.exist?("documents")
  puts "Documents folder exists"
else
  puts "Documents folder does not exist"
end
```

上面的代码将会检查当前目录下是否存在一个名为"documents"的目录，并根据结果打印不同的消息。

深入解析：
在上面的例子中，我们使用了Dir.exist? 方法来检查目录是否存在。这个方法其实是一个类方法，它属于Dir类。Dir类是Ruby标准库中用于处理目录和文件的类，在我们使用时无需额外引入。

此外，除了Dir.exist? 方法外，你还可以使用Dir.exists? 方法来完成相同的任务。这两个方法基本上是相同的，只是exists? 方法是在Ruby 1.9之后引入的新方法，它更易于理解和使用。

总结：
通过在你的代码中加入检查目录是否存在的步骤，你可以有效地避免因为找不到目录而导致的程序错误。同时学习使用Dir.exist? 和Dir.exists? 方法也会为你后续写文件和查找文件的代码带来便利。

参考链接：
- Ruby官方文档：https://ruby-doc.org/core-2.5.1/Dir.html#method-c-exists-3F
- Ruby编程语言：https://www.ruby-lang.org/zh_cn/
- Ruby教程：https://www.runoob.com/ruby/ruby-tutorial.html

参见：
- [检查Ruby文件是否存在](https://www.rubyguides.com/2018/10/file-exists/)
- [从文件路径中提取文件名](https://www.rubyguides.com/2018/10/file-basename/)
- [在Ruby中创建文件](https://www.rubyguides.com/2018/10/create-file-ruby/)