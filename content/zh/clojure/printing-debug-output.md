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

# 这是什么 & 为什么?
打印调试输出是一种程序员用来检查代码中的错误和问题的方法。它可以帮助我们理解程序在运行过程中发生了什么，并且可以提供有用的信息来排除错误。因此，打印调试输出在开发过程中是非常有用的。

# 如何:
```Clojure
(defn add [a b]
  (println "a is" a "and b is" b)
  (+ a b))

(add 3 5)
```

```
a is 3 and b is 5
8
```

在上面的例子中，我们定义了一个函数add，它接受两个参数并返回它们的和。我们通过打印参数的值来调试这个函数，并且最后返回他们的和。这样，当我们运行add函数时，我们就可以看到参数的值，并且确定它们是否正确。

# 深入了解:
打印调试输出的历史可以追溯到早期的程序开发。在过去，程序员使用print语句来输出调试信息，但这种方法很快被发现是低效且容易产生错误的。而今，许多编程语言都提供了专门的调试工具来帮助程序员更加有效地调试代码。在Clojure中，除了求值打印，我们也可以使用clojure.tools.logging库来打印调试信息。

除了打印调试输出，程序员还可以使用断言、日志记录等方法来调试代码。这些方法都有各自的优点和用途，但打印调试输出仍然是最简单和最常用的方法。

# 参考链接:
- [Clojure tools logging](https://clojure.github.io/tools.logging/)
- [Debugging techniques](https://stackoverflow.com/questions/441547/how-can-i-debug-a-code)
- [Assertions in Clojure](https://clojuredocs.org/clojure.core/assert)