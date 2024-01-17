---
title:                "读取命令行参数"
html_title:           "Clojure: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Clojure中的命令行参数

## 什么是命令行参数？为什么程序员要读取它？

命令行参数是指在程序运行时通过命令行输入的额外信息。程序员经常使用命令行参数来控制程序的行为，比如指定不同的文件名、设置不同的选项等。这样可以使程序更加灵活、易于使用。

## 如何读取命令行参数？

要读取命令行参数，可以使用Clojure中的`(command-line)`函数。该函数会返回一个字符串数组，其中包含了所有输入的命令行参数。比如，如果在命令行输入`clojure my-program.clj arg1 arg2`，那么`(command-line)`将返回`["my-program.clj" "arg1" "arg2"]`。

```Clojure
(defn handle-args [args]
  (println "The first argument is:" (nth args 0))
  (println "The second argument is:" (nth args 1))
  ;; do something with the arguments
  )

(handle-args (command-line))
```

输出：

```Clojure
The first argument is: my-program.clj
The second argument is: arg1
```

## 深入了解

读取命令行参数在计算机科学中有着悠久的历史。早期的命令行界面即是通过命令行参数来进行交互的。现在除了使用Clojure的`(command-line)`函数外，还可以使用Java的`java.lang.System`类来读取命令行参数。另外，还有一些命令行参数解析库可供程序员使用，比如Clojure自带的`clojure.tools.cli`库。

## 相关资源

- `(clojure command-line)`官方文档：https://clojuredocs.org/clojure.core/command-line
- `(java.lang.System):getProperties`官方文档：https://docs.oracle.com/javase/8/docs/api/java/lang/System.html
- `clojure.tools.cli`官方文档：https://github.com/clojure/tools.cli