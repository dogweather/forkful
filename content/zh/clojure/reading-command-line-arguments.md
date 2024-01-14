---
title:                "Clojure: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

有时候，在编写程序的过程中，我们需要从命令行中获取参数来控制程序的行为。这样可以让程序更加灵活，并且可以根据不同的参数做出不同的响应。因此，学习如何读取命令行参数是非常重要的。

## 如何

使用Clojure读取命令行参数非常简单，只需要使用`command-line-args`函数就可以了。例如，我们想要从命令行中获取用户名和密码，可以编写如下代码：

```Clojure
(def username (nth *command-line-args* 0))
(def password (nth *command-line-args* 1))
```

然后在命令行中运行程序时，可以通过在程序名后面加上用户名和密码来传递参数。例如：`java -jar myprogram.jar alice 123456`。程序就会将`alice`和`123456`分别赋值给`username`和`password`变量。

可以看到，使用`command-line-args`函数可以轻松地读取命令行参数，并且还可以通过索引来获取不同的参数值。

## 深入探讨

除了使用`command-line-args`函数读取命令行参数之外，Clojure还提供了`*command-line-args*`变量来获取所有的命令行参数。这个变量是一个字符串列表，可以通过`nth`函数来读取其中的值。

另外，我们也可以通过使用`clojure.java.io/files`命名空间中的`command-line`函数来读取命令行参数。该函数会返回一个关联数组，包含了所有的命令行参数及其对应的值。

总的来说，读取命令行参数在Clojure中是非常简单的，我们可以根据自己的需求来选择使用哪种方式。

## 另请参阅

- [Clojure官方文档](https://clojure.org/reference/java_interop#command-line-options)
- [如何编写命令行程序（英文）](https://hackernoon.com/how-to-make-a-command-line-program-in-clojure-4c8d1f8a2a19)
- [命令行参数的处理（英文）](https://purelyfunctional.tv/article/reading-command-line-args-clojure/)