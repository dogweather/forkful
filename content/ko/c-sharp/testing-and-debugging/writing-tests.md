---
title:                "테스트 작성하기"
aliases:
- /ko/c-sharp/writing-tests/
date:                  2024-02-03T19:30:23.786852-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
