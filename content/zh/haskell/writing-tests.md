---
title:                "编写测试"
html_title:           "Haskell: 编写测试"
simple_title:         "编写测试"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# 什么是写测试

写测试就是编写一些程序来检验我们的代码是否能够按照预期工作。程序员们这样做的原因是为了确保我们的代码质量，避免出现意外的bug，减少修复错误所需要的时间。

## 如何编写测试

以下是一个简单的示例代码，在Haskell中编写测试非常容易，我们可以使用hspec库来编写并运行测试。

```Haskell
testSum :: IO ()
testSum = hspec $ do
  describe "sum" $ do
    it "correctly sums up two integers" $ do
      sum 1 2 `shouldBe` 3
    it "correctly sums up a list of integers" $ do
      sum [1, 2, 3] `shouldBe` 6
```

运行以上代码，我们将得到下面的输出结果：

```
sum
  - correctly sums up two integers
  - correctly sums up a list of integers

Finished in 0.0001 seconds
2 examples, 0 failures
```

我们可以看到，通过编写测试，我们可以很容易地检验我们的代码是否按照预期工作。

## 深入了解

写测试最早是在软件开发的早期阶段出现的一个重要实践。它可以帮助程序员们在代码量增大后保持代码的质量，并且保证新的修改不会破坏现有的功能。除了Haskell的hspec库之外，其他编程语言也有自己的测试框架，例如Python的unittest和Java的JUnit。

## 相关链接

了解更多关于Haskell中测试的信息，请查看hspec文档：http://hspec.github.io/

欢迎探索其他编程语言的测试框架，慢慢发现自己喜欢的方法。