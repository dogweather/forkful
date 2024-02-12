---
title:                "生成随机数"
aliases: - /zh/vba/generating-random-numbers.md
date:                  2024-02-01T21:54:15.045451-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Visual Basic for Applications（VBA）中生成随机数可以让程序模拟具有机会或可变性元素的过程，例如掷骰子或抽样数据。程序员使用这些技术来开发模型、游戏或者在预测结果不现实或不那么有用的情况下的模拟。

## 如何操作：

在VBA中，使用`Rnd`函数来生成随机数。默认情况下，`Rnd`生成一个大于等于0且小于1的单精度浮点数。以下是一些步骤和示例，以有效利用随机数：

1. **简单随机数：**
   要生成一个基本的随机数，你只需要调用`Rnd()`：

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' 0到1之间的随机数
       MsgBox randomNumber
   End Sub
   ```

2. **设置种子：**
   `Randomize`语句初始化随机数生成器，这对于确保每次运行VBA代码时都有不同的结果至关重要：

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **生成范围内的数字：**
   通常，你会希望生成一个特定范围内的随机数。以下是如何生成1到100之间的数字：

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' 1到100之间的随机数
       MsgBox randomNumber
   End Sub
   ```

### 示例输出：
运行`RandomNumberInRange`后，你可能会看到一个消息框显示像`45`这样的数字。

## 深入探讨：

VBA中的`Rnd`函数虽然易于使用，但实际上是基于确定性算法生成伪随机数的。这意味着它产生的数字序列并非真正随机，但通常对于需要随机过程的常见任务而言已经足够。

历史上，VBA中的随机数生成能力可以追溯到Basic的早期版本，随着时间的推移适应变化，包括像`Randomize`这样的特性以通过提供一个起点来改善随机性。然而，对于需要高随机性水平的应用程序，如安全的加密操作，VBA的`Rnd`可能不是最佳工具。应该考虑在更健壮的编程环境或设计有加密意图的语言中的替代方案，如Python的`secrets`模块或Java的`SecureRandom`。

尽管存在限制，但在VBA中生成随机数的简单性和可访问性继续使其成为广泛应用、模拟工作和教育目的的有价值工具。
