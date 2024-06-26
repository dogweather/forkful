---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:18.489334-07:00
description: "\u5982\u4F55\u505A\uFF1A C# \u5F00\u53D1\u8005\u4E3B\u8981\u4F7F\u7528\
  \ NUnit \u6216 xUnit \u6846\u67B6\u6765\u7F16\u5199\u6D4B\u8BD5\uFF0C\u56E0\u4E3A\
  \u5B83\u4EEC\u7684\u7075\u6D3B\u6027\u548C\u5E7F\u6CDB\u7684\u529F\u80FD\u96C6\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528 NUnit \u6D4B\u8BD5\u4E00\u4E2A\u7B80\u5355\
  \u52A0\u6CD5\u51FD\u6570\u7684\u57FA\u672C\u793A\u4F8B\uFF1A 1. **\u901A\u8FC7 NuGet\
  \ \u5305\u7BA1\u7406\u5668\u6216 .NET CLI \u5B89\u88C5 NUnit \u548C NUnit3TestAdapter**\uFF1A\
  ."
lastmod: '2024-04-05T22:38:46.934078-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u505A\uFF1A C# \u5F00\u53D1\u8005\u4E3B\u8981\u4F7F\u7528 NUnit\
  \ \u6216 xUnit \u6846\u67B6\u6765\u7F16\u5199\u6D4B\u8BD5\uFF0C\u56E0\u4E3A\u5B83\
  \u4EEC\u7684\u7075\u6D3B\u6027\u548C\u5E7F\u6CDB\u7684\u529F\u80FD\u96C6\u3002\u8FD9\
  \u91CC\u6709\u4E00\u4E2A\u4F7F\u7528 NUnit \u6D4B\u8BD5\u4E00\u4E2A\u7B80\u5355\u52A0\
  \u6CD5\u51FD\u6570\u7684\u57FA\u672C\u793A\u4F8B\uFF1A 1. **\u901A\u8FC7 NuGet \u5305\
  \u7BA1\u7406\u5668\u6216 .NET CLI \u5B89\u88C5 NUnit \u548C NUnit3TestAdapter**\uFF1A\
  ."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何做：
C# 开发者主要使用 NUnit 或 xUnit 框架来编写测试，因为它们的灵活性和广泛的功能集。这里有一个使用 NUnit 测试一个简单加法函数的基本示例：

1. **通过 NuGet 包管理器或 .NET CLI 安装 NUnit 和 NUnit3TestAdapter**：
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **如果您尚未这样做，请创建一个 C# 类库项目**。

3. **编写一个要测试的简单函数**。例如，在一个名为 `Calculator` 的类中的加法方法：
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **使用 NUnit 编写测试类**：
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // 准备
            var calculator = new Calculator();
            int expected = 5;

            // 执行
            int actual = calculator.Add(2, 3);

            // 验证
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **使用您的 IDE 的测试运行器或 .NET CLI 运行测试**：
```powershell
dotnet test
```

### 示例输出：
假设您的测试通过，您应该看到类似于此的输出：
```
测试运行成功。
总测试数：1
     通过：1
 总时间：1.2345 秒
```

### 使用 xUnit：
如果您更喜欢 xUnit，设置与 NUnit 类似。这是您使用 xUnit 为 `Calculator` 类重写测试示例的方法：

1. **安装 xUnit 和 xUnit.runner.visualstudio**：
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **使用 xUnit 编写测试类**：
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // 准备
            var calculator = new Calculator();
            int expected = 5;

            // 执行
            int actual = calculator.Add(2, 3);

            // 验证
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **使用 .NET CLI 或您的 IDE 集成的测试运行器运行测试**。

NUnit 和 xUnit 都为参数化测试、设置/拆卸操作和将测试组织成类别提供了强大功能，这些都是 C# 程序员工具箱中保证代码质量和功能性的不可或缺的工具。
