---
date: 2024-01-20 17:55:34.101376-07:00
description: "\u600E\u4E48\u505A\uFF1A Clojure\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\
  \u53C2\u6570\u7684\u65B9\u5F0F\u501F\u9274\u4E86Java\u7684\u4F20\u7EDF\uFF0C\u4F46\
  \u4F7F\u5176\u66F4\u7B80\u5355\u3002\u6CA1\u6709`public static void main`\u6807\u51C6\
  \u65B9\u6CD5\uFF1B\u53D6\u800C\u4EE3\u4E4B\u662F`*command-line-args*`\u53D8\u91CF\
  \u3002 \u6709\u51E0\u79CD\u65B9\u6CD5\u53EF\u4EE5\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\
  \u6570\uFF1A - \u4F7F\u7528`*command-line-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.549129-06:00'
model: gpt-4-1106-preview
summary: "\u6709\u51E0\u79CD\u65B9\u6CD5\u53EF\u4EE5\u8BFB\u53D6\u547D\u4EE4\u884C\
  \u53C2\u6570\uFF1A - \u4F7F\u7528`*command-line-args*`\u662F\u6700\u7B80\u5355\u7684\
  \u65B9\u5F0F\uFF0C\u65E0\u9700\u4EFB\u4F55\u5E93\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
