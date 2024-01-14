---
title:                "Clojure: 打印调试输出"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么要打印调试输出？

在编写Clojure程序时，经常会遇到需要查看程序运行过程中的变量值或代码执行情况的情况。这时候，打印调试输出就是一种非常有效的方法。通过打印输出，我们可以更容易地理解程序的运行与逻辑，从而更快地解决问题。

## 如何打印调试输出？
```Clojure
(defn print-debug [x] "```" (println "调试输出：" x) "```")
```
这是一个简单的打印调试输出的函数，将待调试的变量作为参数传入，然后使用`println`函数打印输出。比如我们有一个变量x，需要查看其值，可以使用`(print-debug x)`来进行打印输出。输出的结果将会在控制台上显示出来。

## 深入了解打印调试输出

除了简单地打印变量值，我们还可以使用Clojure的内置函数`pr-str`来将变量转换成字符串再打印输出。这样可以获得更详细的信息，比如变量的类型和内部结构。而使用`pprint`函数则可以进行更美观的输出格式。

需要注意的是，打印调试输出可以帮助我们快速定位问题，但是过多的打印输出会影响程序性能。因此，在调试结束后，记得将打印输出代码删除或注释掉。

## 另请参阅

- [Clojure调试方法](https://clojure.org/guides/debugging)
- [辅助调试的Clojure工具](https://lispcast.com/simple-debugging-tools-in-clojure/)
- [Clojure调试技巧](https://blog.klipse.tech/clojure/2016/06/10/clojure-debugging.html)