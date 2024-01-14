---
title:                "Clojure: 书写测试"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-tests.md"
---

{{< edit_this_page >}}

##为什么

在软件开发过程中，写测试是非常重要的一步。通过编写测试，可以帮助我们发现代码中的错误和潜在问题，有效地保障软件的质量和稳定性。

##如何做

首先，在Clojure中使用內建的`deftest`宏来定义测试函数，以及`is`宏来断言测试的结果。例如：

```Clojure
(deftest test-addition
  (is (= 5 (+ 2 3))))
```

运行测试，可以使用`lein test`命令。如果所有测试都通过，会得到如下输出：

```Clojure
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

若要测试函数抛出预期的异常，可以使用`thrown?`宏，例如：

```Clojure
(deftest test-division
  (is (thrown? ArithmeticException (/ 1 0))))
```

##深入探讨

编写测试时，需要考虑测试的覆盖范围，不要只关注最常见的情况，还要考虑边界情况和异常情况。另外，可以使用`is`宏的`throws`断言来检查函数是否抛出了指定的异常类型，以及`reduced?`宏来检查函数是否返回了`reduced`值。

此外，Clojure还提供了`with-redefs-fn`宏，可以临时重新定义某些函数，以方便测试特定的情况。例如，可以重新定义随机数生成函数，以便于测试随机情况。

##另请参阅

- [Clojure官方文档](https://clojure.org/guides/testing)
- [Clojure测试初探](https://zhuanlan.zhihu.com/p/27165326)
- [使用Clojure的测试框架TestCheck进行属性测试](https://www.baeldung.com/clojure-test-check)