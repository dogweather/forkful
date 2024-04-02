---
date: 2024-01-20 18:03:21.902387-07:00
description: "\u521B\u5EFA\u65B0\u9879\u76EE\u662F\u6784\u5EFA\u5168\u65B0\u8F6F\u4EF6\
  \u7A0B\u5E8F\u7684\u8D77\u70B9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\
  \u4E86\u89E3\u51B3\u95EE\u9898\u3001\u5B9E\u9A8C\u65B0\u60F3\u6CD5\u6216\u5F00\u53D1\
  \u521B\u65B0\u7684\u5DE5\u5177\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.303402-06:00'
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u65B0\u9879\u76EE\u662F\u6784\u5EFA\u5168\u65B0\u8F6F\u4EF6\
  \u7A0B\u5E8F\u7684\u8D77\u70B9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\
  \u4E86\u89E3\u51B3\u95EE\u9898\u3001\u5B9E\u9A8C\u65B0\u60F3\u6CD5\u6216\u5F00\u53D1\
  \u521B\u65B0\u7684\u5DE5\u5177\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

## What & Why? (是什么 & 为什么?)
创建新项目是构建全新软件程序的起点。程序员这么做是为了解决问题、实验新想法或开发创新的工具。

## How to: (如何操作)
Clojure 项目通常使用 Leiningen 或 Clojure CLI 工具开始。Leiningen 是大家都爱的工具。下面是怎么用 Leiningen 创建一个新项目的步骤。

```Clojure
;; 安装 Leiningen（跟随官方指南）

;; 使用 Leiningen 创建新项目
lein new app my-cool-app

;; 结构将类似这样：
;; my-cool-app/
;; ├── project.clj
;; ├── src/
;; │   └── my_cool_app/
;; │       └── core.clj
;; ├── test/
;; │   └── my_cool_app/
;; │       └── core_test.clj
;; └── README.md
```

运行项目：

```Clojure
cd my-cool-app
lein run

;; 输出: Hello, World!
```

## Deep Dive (深入探索)
Clojure 诞生于 2007 年，由 Rich Hickey 开发。它建立在 Lisp 的基础上，以其宏、不可变数据结构和函数式编程风格而闻名。Leiningen 和 Clojure CLI 是创建项目的两种方式。Leiningen 更老练和广泛使用。Clojure CLI 则是一个更新、更轻量级的选择。

Leiningen 采用 `project.clj` 管理依赖。Clojure CLI 使用 `deps.edn`。每个工具都有自己的命令行界面和插件生态系统。

从技术上，创建新项目可以手动完成，但自动化工具会处理许多模板和配置工作，让你专注于代码。

## See Also (另请参阅)
- [Leiningen 的官方网站](https://leiningen.org/)
- [Getting Started with Clojure](https://clojure.org/guides/getting_started)
- [Clojure Style Guide](https://guide.clojure.style/)
- [在 Clojure 中开始新项目的更详细指南](https://clojure.org/guides/getting_started#_creating_a_project)
