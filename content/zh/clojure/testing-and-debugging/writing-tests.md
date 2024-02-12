---
title:                "编写测试"
aliases: - /zh/clojure/writing-tests.md
date:                  2024-02-03T19:30:05.967597-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在 Clojure 中编写测试，如同在其他编程语言中一样，涉及创建专门的代码来验证你的主代码库是否按预期工作。它有助于确保代码的正确性，有利于重构，以及增强代码的稳定性。

## 如何做：
Clojure 利用JVM，支持各种测试框架。然而，一个常用的内置库是 `clojure.test`。这里有一个简单的例子：

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Addition functionality"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
运行这个测试后，你会看到类似于以下的输出：

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

对于那些寻求更丰富功能的选项的人，可以利用像 `Midje` 或 `test.check` 这样的第三方库。这里展示了如何用 Midje 进行类似的测试：

首先，将 Midje 添加到你的 project.clj 依赖中：
```clojure
[midje "1.9.9"]
```

然后，你的 Midje 测试可能看起来像这样：

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Testing addition"
  (add 2 2) => 4
  (add 3 4) => 7)
```

通过 Midje 运行测试后，使用 `lein midje`，输出会显示类似于以下内容：

```
All checks (2) succeeded.
```
