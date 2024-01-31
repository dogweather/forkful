---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
测试是用来检查代码功能是否正确的方法。编程时写测试能提升代码质量，避免未来出错。

## 如何做：
```Clojure
;; 引入测试库
(require '[clojure.test :refer :all])

;; 定义一个测试
(deftest test-addition
  (testing "加法功能"
    (is (= 4 (+ 2 2)))
    (is (= 7 (+ 3 4)))))

;; 运行测试
(run-tests)

;; 输出结果示例
;; 
;; Testing user
;; 
;; Ran 1 tests containing 2 assertions.
;; 0 failures, 0 errors.
```

## 深入探讨
在历史上，测试一直是软件开发的重要部分。Clojure 测试可以使用内置的库 `clojure.test`，也可选择其他如 `Midje` 或 `Speclj`。测试可以执行单元测试、集成测试等多种方式，是通过测试套件自动运行的。

## 参考资料
- Clojure 官方文档: https://clojure.org/guides/getting_started
- clojure.test API 文档: https://clojuredocs.org/clojure.test
- 关于 Midje 的信息: https://github.com/marick/Midje
- Speclj 项目页: http://speclj.com/
