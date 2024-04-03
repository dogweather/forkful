---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:23.786852-07:00
description: "C#\uC5D0\uC11C \uD14C\uC2A4\uD2B8 \uC791\uC131\uC740 \uCF54\uB4DC\uC758\
  \ \uAE30\uB2A5\uC744 \uAC80\uC99D\uD558\uAE30 \uC704\uD574 \uC790\uB3D9 \uC2A4\uD06C\
  \uB9BD\uD2B8\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uB9D0\uD558\uBA70, \uC608\
  \uC0C1\uB300\uB85C \uB3D9\uC791\uD558\uB294\uC9C0 \uD655\uC778\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uBC84\uADF8\uB97C \uC870\uAE30\uC5D0\
  \ \uBC1C\uACAC\uD558\uACE0, \uCF54\uB4DC \uB9AC\uD329\uD1A0\uB9C1\uC744 \uC6A9\uC774\
  \uD558\uAC8C \uD558\uBA70, \uC0C8\uB85C\uC6B4 \uBCC0\uACBD\uC0AC\uD56D\uC774 \uAE30\
  \uC874 \uAE30\uB2A5\uC744 \uAE68\uB728\uB9AC\uC9C0 \uC54A\uB3C4\uB85D \uD558\uC5EC\
  \ \uC18C\uD504\uD2B8\uC6E8\uC5B4\uC758 \uD488\uC9C8\uACFC \uC2E0\uB8B0\uC131\uC744\
  \u2026"
lastmod: '2024-03-13T22:44:55.238928-06:00'
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uD14C\uC2A4\uD2B8 \uC791\uC131\uC740 \uCF54\uB4DC\uC758\
  \ \uAE30\uB2A5\uC744 \uAC80\uC99D\uD558\uAE30 \uC704\uD574 \uC790\uB3D9 \uC2A4\uD06C\
  \uB9BD\uD2B8\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uB9D0\uD558\uBA70, \uC608\
  \uC0C1\uB300\uB85C \uB3D9\uC791\uD558\uB294\uC9C0 \uD655\uC778\uD569\uB2C8\uB2E4\
  ."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 무엇 & 왜?

C#에서 테스트 작성은 코드의 기능을 검증하기 위해 자동 스크립트를 생성하는 것을 말하며, 예상대로 동작하는지 확인합니다. 프로그래머들이 버그를 조기에 발견하고, 코드 리팩토링을 용이하게 하며, 새로운 변경사항이 기존 기능을 깨뜨리지 않도록 하여 소프트웨어의 품질과 신뢰성을 높이기 위해 이를 수행합니다.

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
