---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:13:15.216774-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
REPL，即读取-求值-打印循环，是一个用于动态测试Clojure代码的编程环境，逐块进行。编程人员使用它来立即反馈、迭代开发和快速实验，无需编译或设置完整的项目环境的开销。

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
