---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:43.319476-07:00
description: "\u5728 Haskell \u4E2D\u7F16\u5199\u6D4B\u8BD5\u662F\u4E3A\u4E86\u901A\
  \u8FC7\u81EA\u52A8\u5316\u68C0\u67E5\u786E\u4FDD\u4F60\u7684\u51FD\u6570\u6309\u9884\
  \u671F\u5DE5\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\
  \u65E9\u6355\u83B7\u9519\u8BEF\uFF0C\u4FC3\u8FDB\u91CD\u6784\uFF0C\u5E76\u8BB0\u5F55\
  \u884C\u4E3A\uFF0C\u4F7F\u4EE3\u7801\u5E93\u66F4\u6613\u4E8E\u7EF4\u62A4\u548C\u6269\
  \u5C55\u3002"
lastmod: '2024-03-13T22:44:47.819236-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Haskell \u4E2D\u7F16\u5199\u6D4B\u8BD5\u662F\u4E3A\u4E86\u901A\u8FC7\
  \u81EA\u52A8\u5316\u68C0\u67E5\u786E\u4FDD\u4F60\u7684\u51FD\u6570\u6309\u9884\u671F\
  \u5DE5\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\
  \u6355\u83B7\u9519\u8BEF\uFF0C\u4FC3\u8FDB\u91CD\u6784\uFF0C\u5E76\u8BB0\u5F55\u884C\
  \u4E3A\uFF0C\u4F7F\u4EE3\u7801\u5E93\u66F4\u6613\u4E8E\u7EF4\u62A4\u548C\u6269\u5C55\
  \u3002."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

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
