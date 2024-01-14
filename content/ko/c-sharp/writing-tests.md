---
title:                "C#: 테스트 작성하기"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

Why:

프로그래밍을 하다보면 오류를 발견할 때마다 수정하는 데 시간이 많이 소비됩니다. 이를 방지하기 위해 코드를 테스트하는 것은 매우 중요합니다. 마찬가지로 C# 코드를 작성할 때도 테스트를 적절하게 작성하는 것이 필수입니다. 이 포스트에서는 왜 테스트 작성이 중요한지에 대해 알아보겠습니다.

How To:

테스트 작성을 시작하기 전에, NUnit과 같은 단위 테스트 프레임워크를 사용한다고 가정하겠습니다. 첫 번째로 할 일은 간단한 클래스를 작성하는 것입니다. 아래는 단순한 계산기 클래스의 예시입니다.

```C#
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }

    public int Subtract(int a, int b)
    {
        return a - b;
    }
}
```

이제 테스트를 작성해보겠습니다. 테스트 메서드는 `[Test]` 어트리뷰트로 시작하고, 그 밑에 기대하는 결과를 `Assert` 문으로 나타내면 됩니다. 아래는 위에서 작성한 계산기 클래스의 `Add` 메서드를 테스트하는 예시입니다.

```C#
[Test]
public void Add_SimpleNumbers_ReturnsCorrectResult()
{
    // Arrange
    Calculator calculator = new Calculator();

    // Act
    int result = calculator.Add(2, 3);

    // Assert
    Assert.AreEqual(5, result);
}
```

위와 같이 테스트를 작성하면 `Calculator` 클래스의 `Add` 메서드가 예상한대로 작동하는지 확인할 수 있습니다. 이제 `Subtract` 메서드도 마찬가지 방식으로 테스트해보겠습니다.

```C#
[Test]
public void Subtract_SimpleNumbers_ReturnsCorrectResult()
{
    // Arrange
    Calculator calculator = new Calculator();

    // Act
    int result = calculator.Subtract(5, 3);

    // Assert
    Assert.AreEqual(2, result);
}
```

위와 같이 간단한 테스트를 작성하는 것만으로도 코드의 오류를 신속하게 발견할 수 있습니다.

Deep Dive:

단위 테스트는 코드를 작성하는 동시에 함께 작성하는 것이 좋습니다. 이렇게 하면 코드에 대한 이해도가 높아지고, 테스트 작성에도 많은 시간을 절약할 수 있습니다. 또한 테스트 작성을 중심으로 코드를 작성하면 점진적인 개발이 가능해집니다. 즉, 작은 단계로 코드를 작성하고 테스트를 추가하는 방식으로 코드를 구현할 수 있습니다.

또한, 테스트를 작성할 때는 모든 시나리오를 고려하는 것이 중요합니다. 예상하지 못한 예외 사항에 대해서도 테스트를 작성하면 코드의 견고성이 더욱 높아집니다. 따라서 가능한 모든 경우의 수를 고려하여 테스트 커버리지를 높이는 것이 좋습니다.

See Also:

 - [NUnit 공식 문서](https://nunit.org/)
 - [단위 테스트를 지탱하는 기술](https://www.aladin.co.kr/shop/wproduct.aspx?ItemId=280480332)