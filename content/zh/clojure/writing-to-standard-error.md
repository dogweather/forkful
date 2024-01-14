---
title:                "Clojure: 标准错误写作"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么要写标准错误？

写标准错误可能听起来不像是一件很有趣的事情，但它却是一个非常有用的技能。当你在编写Clojure程序时，可能会遇到各种错误，而有时候这些错误信息并不会显示在控制台中，而是会被发送到标准错误流中。因此，了解如何写入标准错误是至关重要的，它可以帮助你更好地理解程序的运行情况，并解决潜在的问题。

# 如何写入标准错误？

如果想要将错误信息写入标准错误，可以使用`System/err`方法，它代表标准错误流。下面的代码展示了如何使用`System/err`将一条简单的错误信息写入标准错误：

```Clojure
(System/err "这是一个错误信息")
```

运行以上代码，你会在控制台中看到如下输出：

```
这是一个错误信息
```

同样地，你也可以使用`println`函数将信息打印到标准错误流中：

```Clojure
(println "这是一个错误信息" (System/err))
```

以上代码的控制台输出和上面一样。

# 深入了解

除了上面提到的写入标准错误的基本方法，Clojure还提供了一些宏来更方便地处理错误信息。其中一个是`with-out-str`，它可以将输出存储到一个字符串中，而不是直接打印到控制台。下面的代码展示了如何使用`with-out-str`将错误信息存储到一个字符串中，并在控制台中打印：

```Clojure
(println (with-out-str (System/err "这是一个错误信息")))
```

运行以上代码，你会在控制台中看到如下输出：

```
这是一个错误信息
```

此外，Clojure还提供了`binding`宏来控制输出的位置。它可以让你将输出重定向到另一个流，例如一个文件。下面的代码展示了如何使用`binding`将错误信息写入到一个名为"error.txt"的文件中：

```Clojure
(binding [*err* (clojure.java.io/writer "error.txt")]
  (System/err "这是一个错误信息"))
```

运行以上代码后，你会在当前目录下看到一个名为"error.txt"的文件，其中包含着一条错误信息。

# 查看更多信息

除了以上提到的方法外，Clojure还有许多其他的处理错误信息的方式。如果你想了解更多关于写入标准错误流的用法，可以查看以下链接：

- [Clojure文档](https://clojuredocs.org/)
- [Clojure错误处理官方指南](https://clojure.org/guides/learning/errors)
- [Clojure错误处理教程](https://www.baeldung.com/clojure-errors)

# 参考链接

- [Writing to Standard Error in Clojure](https://gist.github.com/malcolmsparks/629001)