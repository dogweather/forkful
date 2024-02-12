---
title:                "编写测试"
aliases: - /zh/haskell/writing-tests.md
date:                  2024-02-03T19:30:43.319476-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Haskell 中编写测试是为了通过自动化检查确保你的函数按预期工作。程序员这样做是为了尽早捕获错误，促进重构，并记录行为，使代码库更易于维护和扩展。

## 如何进行：

Haskell 支持各种测试框架，但两个流行的是 `Hspec` 和 `QuickCheck`。Hspec 允许你为代码定义人类可读的规范，而 QuickCheck 让你通过描述代码应满足的属性来自动生成测试。

### 使用 Hspec

首先，在你的构建工具配置中添加 `hspec`（例如，`stack.yaml` 或 `cabal` 文件）。然后，导入 `Test.Hspec` 并将测试写为规范：

```haskell
-- 文件: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "能够添加两个数字" $
    add 1 2 `shouldBe` 3

  it "当加零时返回第一个数字" $
    add 5 0 `shouldBe` 5
```

然后，使用你的构建工具运行测试，结果可能如下所示：

```
MyLib.add
  - 能够添加两个数字
  - 当加零时返回第一个数字

完成于 0.0001 秒
2 个示例，0 失败
```

### 使用 QuickCheck

使用 QuickCheck，你表达函数应满足的属性。将 `QuickCheck` 添加到你的项目配置中，然后导入它：

```haskell
-- 文件: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

运行这些测试将自动生成输入以检查指定的属性：

```
+++ OK, 通过了 100 个测试。
+++ OK, 通过了 100 个测试。
```

在 Hspec 和 QuickCheck 示例中，测试套件都充当了可以自动验证代码正确性的可执行文档。
