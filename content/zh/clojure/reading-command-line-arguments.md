---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---

## 什么 & 为什么?

命令行参数让你的程序更灵活，能基于用户输入作出反应。它可以从启动脚本中获取一些数据，比如文件名或者配置选项。

## 怎么做:

Clojure 用 `*command-line-args*` 去获取传递给它的命令行参数。以下是一个简单的例子：

```Clojure
(defn -main [& args]
  (println "命令行参数是: " args))

(defn run []
  (println "你输入的参数是: " *command-line-args*))

(defn -main [& args]
  (run))
```

如果你运行这个程序，比如说 `lein run -- Hello World`，你将看到以下输出：

```Clojure
你输入的参数是: (Hello World)
```

## 深入探索:

在许多门语言中，读取命令行参数都是常见的操作。在 Unix 或者 Linux 底下，这是通过 shell 命令，或者是 C 程序中的 main 函数的参数来完成的。Clojure 虽然不同，但是还是继承了这个传统。

然而你可以在 Clojure 中使用其他的方式来读取命令行参数，比如说使用 `tools.cli` 库，或者其他类似的库。此外， `*command-line-args*` 不仅仅是一个全局变量，它也是一个线程精确的变量，这意味着你可以在不影响其他部分代码的情况下修改它。

## 参考链接:

1. [Clojure - Getting Started](https://clojure.org/guides/getting_started)
2. [tools.cli Github](https://github.com/clojure/tools.cli)
3. [unix - Command Line Arguments](https://unix.stackexchange.com/questions/31414/how-can-i-pass-a-command-line-argument-into-a-shell-script)