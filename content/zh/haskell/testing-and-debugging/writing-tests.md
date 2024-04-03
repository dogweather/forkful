---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:43.319476-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A Haskell \u652F\u6301\u5404\u79CD\u6D4B\
  \u8BD5\u6846\u67B6\uFF0C\u4F46\u4E24\u4E2A\u6D41\u884C\u7684\u662F `Hspec` \u548C\
  \ `QuickCheck`\u3002Hspec \u5141\u8BB8\u4F60\u4E3A\u4EE3\u7801\u5B9A\u4E49\u4EBA\
  \u7C7B\u53EF\u8BFB\u7684\u89C4\u8303\uFF0C\u800C QuickCheck \u8BA9\u4F60\u901A\u8FC7\
  \u63CF\u8FF0\u4EE3\u7801\u5E94\u6EE1\u8DB3\u7684\u5C5E\u6027\u6765\u81EA\u52A8\u751F\
  \u6210\u6D4B\u8BD5\u3002 #."
lastmod: '2024-03-13T22:44:47.819236-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u652F\u6301\u5404\u79CD\u6D4B\u8BD5\u6846\u67B6\uFF0C\u4F46\u4E24\
  \u4E2A\u6D41\u884C\u7684\u662F `Hspec` \u548C `QuickCheck`\u3002Hspec \u5141\u8BB8\
  \u4F60\u4E3A\u4EE3\u7801\u5B9A\u4E49\u4EBA\u7C7B\u53EF\u8BFB\u7684\u89C4\u8303\uFF0C\
  \u800C QuickCheck \u8BA9\u4F60\u901A\u8FC7\u63CF\u8FF0\u4EE3\u7801\u5E94\u6EE1\u8DB3\
  \u7684\u5C5E\u6027\u6765\u81EA\u52A8\u751F\u6210\u6D4B\u8BD5."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

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
