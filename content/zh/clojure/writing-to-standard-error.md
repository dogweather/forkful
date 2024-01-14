---
title:    "Clojure: 标准错误写入"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 为什么会有标准错误的编写

在编程过程中，我们经常会遇到各种各样的错误。有时候，我们需要输出某些信息来帮助我们调试代码。这时，就可以使用标准错误的编写。通过将错误信息输出到标准错误流，我们可以在程序运行过程中及时地捕获错误信息，从而更有效地解决问题。

## 如何进行标准错误的编写

标准错误的编写可以通过Clojure语言提供的`System/err`方法来实现。我们可以使用`println`函数来将想要输出的信息作为参数传入`System/err`，从而将错误信息输出到标准错误流。

```Clojure
(System/err (println "这是一条错误信息"))
```

输出示例：

```
这是一条错误信息
```

## 深入了解标准错误的编写

除了将错误信息输出到屏幕上，我们也可以将它们保存到日志文件中。通过设置Java虚拟机的`-XX:ErrorFile`参数，我们可以指定错误信息的输出路径和文件名。

另外，我们也可以根据具体的需要，将不同等级的错误信息输出到不同的流中。例如，将重要的错误信息输出到标准错误流，而将普通的警告信息输出到标准输出流。

# 参考资料

- [Clojure官方文档 - System/err方法](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/System/err)
- [Java命令行参数文档](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/java.html#BGBJDEGD)
- [深入理解标准输出与标准错误的区别](https://stackoverflow.com/questions/363863/what-is-the-difference-between-system-out-println-and-system-err-println)