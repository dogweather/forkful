---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:05.967597-07:00
description: "\u5982\u4F55\u505A\uFF1A Clojure \u5229\u7528JVM\uFF0C\u652F\u6301\u5404\
  \u79CD\u6D4B\u8BD5\u6846\u67B6\u3002\u7136\u800C\uFF0C\u4E00\u4E2A\u5E38\u7528\u7684\
  \u5185\u7F6E\u5E93\u662F `clojure.test`\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\
  \u5355\u7684\u4F8B\u5B50\uFF1A."
lastmod: '2024-03-13T22:44:47.306564-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u5229\u7528JVM\uFF0C\u652F\u6301\u5404\u79CD\u6D4B\u8BD5\u6846\u67B6\
  \u3002\u7136\u800C\uFF0C\u4E00\u4E2A\u5E38\u7528\u7684\u5185\u7F6E\u5E93\u662F `clojure.test`\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF1A."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

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
