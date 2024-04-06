---
date: 2024-01-26 04:13:15.216774-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A REPL\u662FLisp\u5BB6\u65CF\u4EA4\u4E92\
  \u5F0F\u5F00\u53D1\u54F2\u5B66\u7684\u5173\u952E\uFF0C\u800CClojure\uFF0C\u4E00\u4E2A\
  \u73B0\u4EE3Lisp\u65B9\u8A00\uFF0C\u5145\u5206\u5229\u7528\u4E86\u8FD9\u4E00\u5DE5\
  \u5177\u3002\u5B83\u53EF\u4EE5\u8FFD\u6EAF\u523020\u4E16\u7EAA50\u5E74\u4EE3\u540E\
  \u671F\u7684\u9996\u4E2ALisp\u2026"
lastmod: '2024-04-05T21:53:47.656244-06:00'
model: gpt-4-0125-preview
summary: "Clojure REPL\u4F1A\u8BDD\u53EF\u4EE5\u96C6\u6210\u5230\u5404\u79CD\u73AF\
  \u5883\u4E2D\uFF0C\u5982\u547D\u4EE4\u884C\u3001IDE\uFF08\u4F8B\u5982IntelliJ\u4E2D\
  \u7684Cursive\uFF0C\u6216Emacs\u4E2D\u7684CIDER\uFF09\uFF0C\u6216\u57FA\u4E8E\u6D4F\
  \u89C8\u5668\u7684\u5DE5\u5177\uFF0C\u5982Nightcode\u3002\u66F4\u6DF1\u5C42\u6B21\
  \u6765\u8BF4\uFF0CREPL\u4F7F\u5F00\u53D1\u4EBA\u5458\u80FD\u591F\u5728\u8FD0\u884C\
  \u65F6\u64CD\u7EB5\u8BED\u8A00\u6784\u9020\uFF0C\u5E76\u5728\u5404\u79CD\u8F6C\u6362\
  \u4E2D\u643A\u5E26\u72B6\u6001\uFF0C\u901A\u5E38\u5BFC\u81F4\u63A2\u7D22\u6027\u7F16\
  \u7A0B\u548C\u66F4\u5065\u58EE\u7684\u4EE3\u7801."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 如何操作：
从启动REPL开始：

```Clojure
user=> (println "Hello, REPL!")
Hello, REPL!
nil
```

定义一个函数并尝试使用它：
```Clojure
user=> (defn greet [name] (str "Hello, " name "!"))
#'user/greet
user=> (greet "Clojure 程序员")
"Hello, Clojure 程序员!"
```

用数据结构进行实验：
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## 深入探讨
REPL是Lisp家族交互式开发哲学的关键，而Clojure，一个现代Lisp方言，充分利用了这一工具。它可以追溯到20世纪50年代后期的首个Lisp REPL。其他语言的替代品包括Python的解释器和Node.js的控制台，但Clojure的REPL具有一流的地位，并且是工作流程的不可分割的部分。

Clojure REPL会话可以集成到各种环境中，如命令行、IDE（例如IntelliJ中的Cursive，或Emacs中的CIDER），或基于浏览器的工具，如Nightcode。更深层次来说，REPL使开发人员能够在运行时操纵语言构造，并在各种转换中携带状态，通常导致探索性编程和更健壮的代码。

REPL的功能在如`lein repl`或`clj`这样的工具上体现得淋漓尽致，这些工具允许依赖管理、各种插件和项目特定的自定义，导致一个更高效和灵活的开发过程。

## 另请参阅
- 官方Clojure网站上的REPL指南：https://clojure.org/guides/repl/introduction
- Rich Hickey关于REPL驱动开发的讲话：https://www.youtube.com/watch?v=Qx0-pViyIDU
- 实用Clojure：使用REPL进行迭代开发：http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
