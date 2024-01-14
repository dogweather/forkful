---
title:    "Clojure: 编写测试"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么

在编写Clojure程序时，写测试十分重要。通过编写测试，可以确保代码的正确性，并且可以在后续维护和修改中避免出现意外的bug。

# 如何做

写测试需要使用Clojure内置的测试框架clojure.test。首先，在代码中引入clojure.test库：  
```Clojure
(require '[clojure.test :refer :all]) 
```
接下来，我们可以使用`deftest`宏来创建一个测试用例。例如，我们要测试一个简单的加法函数`add`，我们可以这样编写测试用例：  
```Clojure
(deftest test-addition
  (testing "addition of two positive numbers"
    (is (= 4 (add 2 2))))
  (testing "addition of a negative and a positive number"
    (is (= -3 (add -5 2))))
  (testing "addition of two negative numbers"
    (is (= -8 (add -4 -4)))))
```
上述代码中，我们使用`testing`宏来定义每个测试用例的名称，然后使用`is`宏来断言测试结果是否符合预期。当所有的测试用例都运行通过时，我们可以看到以下输出：  
```Clojure
Testing test-addition

Ran 3 tests containing 3 assertions.
0 failures, 0 errors.
```

除了使用`is`宏外，我们也可以使用`isnt`宏来判断测试结果是否与预期不同。此外，clojure.test还提供了其他一些有用的断言宏，比如`are`用于多个值的断言，`throws?`用于断言是否抛出了异常等等。更多详细的使用方法，请参考[官方文档](https://clojure.github.io/clojure/clojure.test-api.html)。

# 深入

写测试有助于提高代码质量和可维护性。通过编写多个测试用例覆盖各种情况，可以帮助发现潜在的bug。此外，测试也可以作为代码文档的一部分，可以帮助理解函数的输入输出预期。

在编写测试时，还需要注意一些细节。比如，测试用例应该是独立的，不能依赖其他测试的结果。同时，测试也应该是可重复的，即多次运行测试结果始终一致。

总的来说，写测试可以帮助提高代码质量和开发效率，建议在编写Clojure程序时多加使用。

# 参考链接

- [clojure.test官方文档](https://clojure.github.io/clojure/clojure.test-api.html)
- [Clojure程序员如何写出高质量的测试用例](https://mp.weixin.qq.com/s/8pnoMGt6bI7Ek9FyCp2JfA)
- [测试驱动开发（TDD）简介及在Clojure中的实践](https://zhuanlan.zhihu.com/p/146797117)

# 参见

- [clojure.test官方文档](https://clojure.github.io/clojure/clojure.test-api.html)
- [测试驱动开发（TDD）简介及实践指南](https://mp.weixin.qq.com/s/8pnoMGt6bI7Ek9FyCp2JfA)
- [Clojure程序员如何写出高质量的测试用例](https://zhuanlan.zhihu.com/p/146797117)