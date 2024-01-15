---
title:                "读取命令行参数。"
html_title:           "Clojure: 读取命令行参数。"
simple_title:         "读取命令行参数。"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

读取命令行参数是非常常见的操作，它可以帮助我们控制程序的行为，并向程序传递必要的信息。使用Clojure，我们可以轻松地读取命令行参数，让程序更加灵活和智能。

## 如何使用

假设我们的程序需要接受两个命令行参数，分别是用户的姓名和年龄。我们可以使用“def”命令来声明变量，并使用“(command-line)”函数来获取命令行参数。代码如下所示：

```Clojure
(def name (nth (command-line) 0))
(def age (nth (command-line) 1))
```

然后，我们可以使用“println”函数来打印出用户提供的参数信息：

```Clojure
(println "Hello," name "! You are" age "years old.")
```

运行程序时，我们可以通过在命令行中输入参数来传递信息，例如：

```Clojure
java -jar myprogram.jar John 25
```

这样，程序就会输出“Hello, John! You are 25 years old.”，并根据用户提供的参数来执行相应的逻辑。

## 深入了解

除了使用“(nth (command-line) 0)”这种基本的方法，我们还可以使用Clojure内置的“get”函数来读取命令行参数。例如：

```Clojure
(def args (rest (command-line)))
(def name (get args "--name"))
(def age (get args "--age"))
```

这样，我们只需要输入参数名和对应的值，就可以更加灵活地获取命令行参数。另外，我们也可以使用Java的“System”类来读取命令行参数，这样能够更加灵活地处理异常情况。

## 参考资料

- [Clojure官方文档](https://clojure.org/reference/java_interop#_command_line_interop)
- [优达学城教程：如何读取命令行参数](https://cn.udacity.com/blog/post/clojure-seminar-recap-takeaways-from-live-coding-with-stuart-halloway)
- [Java中System类的使用方法](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html)

## 参见

- [GitHub帮助页面 - 读取命令行参数](https://help.github.com/articles/working-with-the-command-line/)
- [酷壳文章：命令行参数的使用技巧](https://coolshell.cn/articles/4382.html)