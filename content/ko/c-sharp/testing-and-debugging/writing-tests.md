---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:23.786852-07:00
description: "\uBC29\uBC95: C# \uAC1C\uBC1C\uC790\uB4E4\uC740 \uC8FC\uB85C NUnit\uC774\
  \uB098 xUnit \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD14C\
  \uC2A4\uD2B8\uB97C \uC791\uC131\uD558\uB294\uB370, \uADF8 \uC774\uC720\uB294 \uC774\
  \uB4E4\uC758 \uC720\uC5F0\uC131\uACFC \uAD11\uBC94\uC704\uD55C \uAE30\uB2A5 \uC138\
  \uD2B8 \uB54C\uBB38\uC785\uB2C8\uB2E4. \uB2E4\uC74C\uC740 NUnit\uC744 \uC0AC\uC6A9\
  \uD558\uC5EC \uAC04\uB2E8\uD55C \uB367\uC148 \uD568\uC218\uB97C \uD14C\uC2A4\uD2B8\
  \uD558\uB294 \uAE30\uBCF8 \uC608\uC2DC\uC785\uB2C8\uB2E4: 1. **NUnit\uACFC\u2026"
lastmod: '2024-03-13T22:44:55.238928-06:00'
model: gpt-4-0125-preview
summary: "C# \uAC1C\uBC1C\uC790\uB4E4\uC740 \uC8FC\uB85C NUnit\uC774\uB098 xUnit \uD504\
  \uB808\uC784\uC6CC\uD06C\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD14C\uC2A4\uD2B8\uB97C\
  \ \uC791\uC131\uD558\uB294\uB370, \uADF8 \uC774\uC720\uB294 \uC774\uB4E4\uC758 \uC720\
  \uC5F0\uC131\uACFC \uAD11\uBC94\uC704\uD55C \uAE30\uB2A5 \uC138\uD2B8 \uB54C\uBB38\
  \uC785\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
C# 개발자들은 주로 NUnit이나 xUnit 프레임워크를 사용하여 테스트를 작성하는데, 그 이유는 이들의 유연성과 광범위한 기능 세트 때문입니다. 다음은 NUnit을 사용하여 간단한 덧셈 함수를 테스트하는 기본 예시입니다:

1. **NUnit과 NUnit3TestAdapter를 NuGet 패키지 관리자나 .NET CLI를 통해 설치하세요**:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **C# 클래스 라이브러리 프로젝트를 생성하세요** 아직 수행하지 않았다면.

3. **테스트할 간단한 함수를 작성하세요**. 예를 들어, `Calculator`라는 클래스 안에 덧셈 메소드:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **NUnit을 사용하여 테스트 클래스를 작성하세요**:
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
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **IDE의 테스트 러너나 .NET CLI를 사용하여 테스트를 실행하세요**:
```powershell
dotnet test
```

### 샘플 출력:
테스트가 통과한다면, 다음과 비슷한 출력을 볼 수 있습니다:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### xUnit 사용하기:
xUnit을 선호한다면, 설정은 NUnit과 비슷합니다. 다음은 xUnit을 사용하여 `Calculator` 클래스에 대한 테스트 예제를 다시 작성하는 방법입니다:

1. **xUnit과 xUnit.runner.visualstudio를 설치하세요**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **xUnit을 사용하여 테스트 클래스를 작성하세요**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **.NET CLI나 IDE의 통합 테스트 러너를 사용하여 테스트를 실행하세요**.

NUnit과 xUnit은 모두 매개변수화된 테스트, 설정/해제 작업, 테스트를 범주로 조직하는 기능을 제공하여 코드 품질과 기능성을 보장하기 위한 C# 프로그래머의 필수 도구입니다.
