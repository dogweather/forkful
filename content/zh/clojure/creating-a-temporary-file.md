---
title:    "Clojure: 创建临时文件"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么要创建临时文件
在编写Clojure程序时，有时候我们需要在程序运行期间创建临时文件来存储一些临时数据。这可以帮助我们更有效地处理数据，同时也可以降低对系统资源的使用量。

## 如何创建临时文件
在Clojure中创建临时文件非常简单，我们可以使用`java.io.File/createTempFile`函数来创建一个临时文件，并指定文件名和所在目录。例如，下面的代码将在当前目录下创建一个名为“temp.txt”的临时文件，并向其中写入"Hello World!"这个字符串。

```Clojure
(def temp-file (java.io.File/createTempFile "temp" ".txt"))
(spit temp-file "Hello World!")
```

运行上面的代码后，我们可以在当前目录下找到`temp.txt`这个临时文件，并打开查看其中的内容。

## 深入了解临时文件
临时文件在我们的程序运行结束后会自动被删除，这可以避免在系统中留下无用的文件。如果我们需要手动删除临时文件，可以使用`(.deleteOnExit temp-file)`函数来指定临时文件在程序结束时被删除。另外，我们还可以使用`(.setWritable temp-file false)`等函数来操作临时文件的权限。

# 参考资料
- [Clojure官方文档 - java.io.File](https://clojuredocs.org/clojure.java.io/file)
- [Clojure Cookbook - Using temp files](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/08_files/8-11_temporary-files.asciidoc)