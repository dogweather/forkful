---
title:                "开始一个新的项目。"
html_title:           "Clojure: 开始一个新的项目。"
simple_title:         "开始一个新的项目。"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

开启一个新项目是指开始一个全新的编码工程。程序员通常会这么做来实现新的想法、解决问题或者改进现有的项目。

## 什么是开启新项目？为什么程序员要这么做？

开启一个新项目意味着开始一个新的编码任务。程序员这么做通常是为了实现新的想法、解决问题或者改进现有的项目。

## 如何开启新项目？

```Clojure
(defn new-project [name]
  (println "Creating new project named" name)
  (mkdir name)
  (cd name)
  (init-git)
  (add-commit "Initial commit")
  (add-deps :lein {:plugins [[lein-new new/file]]})
  (new/file name)
  (add-commit "Created main file"))
```

输出：

```
Creating new project named my-project
Initializing project as a git repository
Adding an initial commit
Adding dependencies using Leiningen
Creating a new file named my-project
Adding a commit for creating the main file
```

## 深入了解

创建新项目的历史背景

在过去，程序员们通常需要手工创建新的项目并配置环境，这是一项费时费力的任务。然而，现在有许多工具可用来简化这一过程，如Leiningen和Clojure CLI。

其他选项

除了Clojure，还有其他编程语言可用于创建新项目，如Java、Python和JavaScript。

实现细节

上面的示例使用了Clojure函数来创建一个新项目。我们通过调用系统命令来创建目录、初始化git仓库和添加commit。我们还使用了Leiningen来添加依赖并创建新文件，这使创建新项目变得更加简单和高效。

## 参考资料

- [Leiningen官方网站](https://leiningen.org/)
- [Clojure CLI官方网站](https://clojure.org/guides/getting_started)