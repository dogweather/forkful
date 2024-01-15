---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么

在编写Clojure程序时，打印调试输出是一个非常重要的技巧。它可以帮助我们更好地理解程序的运行过程，定位bug，并提高代码的可读性。


# 如何进行调试输出

```Clojure
(defn print-debug [x]
  (println "Debug output:" x))
  
(print-debug "Hello World")
```

输出:

```
Debug output: Hello World
```


代码解释:

在这段代码中，我们定义了一个名为`print-debug`的函数，它接受一个参数`x`。在函数体中，我们使用`println`函数打印出 "Debug output:" 字符串和参数`x`的值。然后，我们调用`print-debug`函数，并传入一个字符串“Hello World”作为参数。最后，我们就可以在控制台中看到调试输出了。

# 深入探讨

除了在函数体中使用`println`函数，我们还可以使用`pr`函数来打印对象的字符串表示。比如，我们可以修改上面的例子：

```Clojure
(defn print-debug [x]
  (pr "Debug output:" x))
  
(print-debug "Hello World")
```

输出：

```
Debug output: "Hello World"
```

`pr`函数打印出来的是字符串的实际值，而`println`打印的则是字符串的显示形式。这在调试复杂数据结构时非常有用，可以帮助我们更好地理解数据的组织结构。

除了简单地打印字符串或对象的值，我们也可以使用Clojure提供的`format`函数来格式化输出。比如：

```Clojure
(defn print-debug [x]
  (println (format "Debug output: %s" x)))
  
(print-debug "Hello World")
```

输出：

```
Debug output: Hello World
```

`format`函数中的`%s`表示字符串的占位符，后面的参数会按照顺序插入到字符串中。这样，我们就可以自定义打印的格式，更方便地进行调试。

# 参考链接

- [Clojure官方文档](https://clojure.org/)
- [Clojure学习资源](https://github.com/clojure-china/learn-clojure)