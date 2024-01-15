---
title:                "编写测试"
html_title:           "Clojure: 编写测试"
simple_title:         "编写测试"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

在写代码时，测试是一个非常重要的步骤。它可以帮助我们验证代码的正确性，提高代码的质量，减少程序中的错误。通过写测试，我们可以更加自信地修改代码，同时也能够帮助我们构建可靠的软件系统。

## 如何做

使用Clojure编写测试非常简单，下面是一个简单的示例：

```Clojure
(ns example-test
  (:require [clojure.test :refer :all]
            [example :refer :all])) ; 导入被测试的命名空间

(deftest test-example ; 定义一个测试函数
  (is (= 2 (add 1 1))) ; 使用is断言测试函数的输出值是否为2
  (is (= 0 (subtract 2 2))))

```

测试函数使用`deftest`宏定义，它的第一个参数是函数的名字，接下来是需要测试的断言。我们可以使用`is`断言来判断函数输出的值是否和预期相等。测试通过时，控制台会显示`Passed`，失败时则会显示失败的断言。

## 深入了解

Clojure提供了许多用于编写测试的宏和函数。除了`deftest`和`is`之外，我们还可以使用`testing`宏来定义一个测试套件，使用`run-test`函数来运行所有的测试，使用`are`宏来测试多个输入和输出值。还有许多其他的工具，可以根据需求选择使用。

除了Clojure内置的测试框架，社区也开发了许多第三方工具，如Leiningen和Midje，它们可以帮助我们更加高效地编写和运行测试。建议读者根据自己的需求和习惯来选择适合自己的工具。

## 参考链接

- [Clojure官方文档](https://clojure.org/guides/testing)
- [Leiningen官方文档](https://leiningen.org/)
- [Midje官方文档](https://github.com/marick/Midje/wiki)

## 了解更多

希望本文让你对Clojure的测试有更清晰的了解。如果你想进一步了解Clojure的其他方面，可以参考下面的链接：

- [Clojure官方文档](https://clojure.org/)
- [Clojure Cookbook](https://www.clojurecookbook.org/)
- [Clojure for the Brave and True](https://www.braveclojure.com/)