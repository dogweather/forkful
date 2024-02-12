---
title:                "编写测试"
aliases:
- /zh/vba/writing-tests.md
date:                  2024-02-01T22:08:42.824955-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

在编程中编写测试包括创建特定程序来验证代码段的功能和性能，确保它们在各种条件下按预期工作。程序员这样做是为了尽早发现错误，提高代码质量，以及便于未来的代码维护和增强。

## 如何操作:

虽然Visual Basic for Applications (VBA) 没有内置的测试框架，类似于Python或JavaScript等语言中可用的那些，但你仍然可以实现简单的测试程序来检查代码的完整性。以下是一个示例：

假设您有一个VBA函数，可以添加两个数字:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

为了测试这个函数，你可以编写另一个过程来验证其输出是否符合预期结果:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "测试通过!", vbInformation
    Else
        MsgBox "测试失败。预期为15但得到 " & result, vbCritical
    End If
End Sub
```

运行`TestAddNumbers`将会显示一个消息框，指示测试是通过还是失败，基于函数的输出。虽然这是一个简化的情景，但您可以通过结合循环、不同的输入值和针对多个函数的测试来构建更复杂的测试。

## 深入了解

这里展示的VBA编写测试的方法是手动的，并且缺乏其他编程环境中可用的更复杂测试框架的特性，如自动测试运行、设置/拆除程序以及集成的测试结果报告。在更广泛采用单元测试框架和测试驱动开发(TDD)之前，类似所描述的手动测试过程很常见。虽然这种方法简单且对于小型项目或学习目的可能是有效的，但对于更大的项目或团队来说，它既不可扩展也不高效。

在支持更丰富的开发工具集的环境中，程序员经常会转向如.NET应用程序的NUnit或Java应用程序的JUnit等框架，这些框架为系统地编写和运行测试提供了全面的工具。这些框架提供高级特性，如断言测试结果、设置模拟对象和测量代码覆盖率。

对于寻求更高级测试能力的VBA开发人员来说，最接近的替代方案可能是利用外部工具或与其他编程环境集成。一些开发人员使用VBA与Excel结合使用，手动记录测试场景和结果。虽然这不像使用专用的测试框架那样方便或自动化，但这些方法可以部分弥补差距，帮助保持VBA解决方案在复杂或关键应用中的可靠性。
