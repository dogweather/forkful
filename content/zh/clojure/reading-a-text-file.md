---
title:                "Clojure: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么阅读文本文件

阅读文本文件是编程中常见的任务，它可以帮助我们获取和整理大量的文本数据。无论是分析日志文件、提取文本信息，还是进行自然语言处理，阅读文本文件都是必不可少的步骤。在Clojure的世界中，我们也可以使用简单而强大的方法来读取文本文件。

# 如何阅读文本文件

在Clojure中，我们可以使用`read-line`函数来逐行读取文本文件。首先，我们需要打开文件并将其绑定到一个变量中：

```Clojure
(def file (clojure.java.io/reader "example.txt"))
```

然后，我们可以使用`read-line`函数来读取每一行，并将其打印出来：

```Clojure
(first (read-line file)) 
```

我们也可以将所有行读取到一个集合中：

```Clojure
(def lines (line-seq file))
```

现在，我们可以对这个集合进行操作，比如过滤、转换等等。最后，记得关闭文件：

```Clojure
(clojure.java.io/close file)
```

# 深入阅读文本文件

除了使用`read-line`函数，我们还可以使用`slurp`函数来一次性读取整个文本文件。该函数将文本文件的内容存储在一个字符串中，因此适用于小型文本文件。

```Clojure
(def text (slurp "example.txt"))
```

如果文本文件比较大，我们也可以使用流(`stream`)来逐行读取文本文件，这样可以避免将整个文件加载到内存中。

另外，我们还可以设置读取文本文件时的编码格式，比如UTF-8或者GB2312。这样可以避免在读取中文文本时出现乱码问题。

# 参考资料

- [Clojure官方文档](https://clojure.org/)
- [Clojure学习资源合集](https://leetcode-cn.com/circle/article/NBmLcF/)