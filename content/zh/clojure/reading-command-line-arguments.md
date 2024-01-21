---
title:                "读取命令行参数"
date:                  2024-01-20T17:55:34.101376-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
命令行参数允许用户影响程序的行为。程序员读取它们，因为这样可以灵活控制程序，在执行时动态传递信息。

## 怎么做：
使用Clojure读取命令行参数非常直接。这里有一些简单的例子：

```Clojure
; 打印所有命令行参数
(defn print-args []
  (doseq [arg *command-line-args*]
    (println arg)))

; 运行程序并传递参数
; $ clj your_script.clj param1 param2
; 输出：
; param1
; param2
```

## 深入探索
Clojure中读取命令行参数的方式借鉴了Java的传统，但使其更简单。没有`public static void main`标准方法；取而代之是`*command-line-args*`变量。

有几种方法可以读取命令行参数：
- 使用`*command-line-args*`是最简单的方式，无需任何库。
- `tools.cli`库提供了更复杂的解析选项，例如标志和选项处理。
- 还可以使用Java的`System/getProperty`来处理系统属性。

细节上，`*command-line-args*`是在程序启动时就设置好的，它包含了运行程序时传递的所有参数，就像一个字符串列表。

## 参见
- Clojure官方文档: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- `tools.cli`库文档: [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)