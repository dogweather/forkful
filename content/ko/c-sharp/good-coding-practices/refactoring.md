---
aliases:
- /ko/c-sharp/refactoring/
date: 2024-01-26 01:17:46.906112-07:00
description: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\
  \uB4DC\uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80\
  \ \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC7AC\uAD6C\uC131\uD558\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uCF54\uB4DC\uB97C \uC815\uB9AC\uD558\uACE0, \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\
  \uC2DC\uD0A4\uACE0, \uBCF5\uC7A1\uC131\uC744 \uC904\uC774\uACE0, \uC720\uC9C0\uBCF4\
  \uC218\uC131\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744\
  \ \uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:06.233175
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\uB4DC\
  \uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80 \uB3D9\
  \uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC7AC\uAD6C\uC131\uD558\uB294\
  \ \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\
  \uB4DC\uB97C \uC815\uB9AC\uD558\uACE0, \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\
  \uD0A4\uACE0, \uBCF5\uC7A1\uC131\uC744 \uC904\uC774\uACE0, \uC720\uC9C0\uBCF4\uC218\
  \uC131\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uD569\
  \uB2C8\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
---

{{< edit_this_page >}}

## 무엇이며 왜?

리팩토링은 기존 컴퓨터 코드의 구조를 변경하지 않고 외부 동작을 변경하지 않고 재구성하는 과정입니다. 프로그래머들은 코드를 정리하고, 가독성을 향상시키고, 복잡성을 줄이고, 유지보수성을 개선하기 위해 이 작업을 합니다.

## 방법:

숫자 배열의 합을 계산하고 출력하는 간단한 C# 메서드를 리팩토링해봅시다:

리팩토링 전:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

리팩토링 후:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// 사용법:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

리팩토링을 통해, 우리는 관심사를 분리하고 `Calculator` 클래스를 더 유연하게 만들어서 어떤 숫자 배열이든 처리할 수 있게 했으며, LINQ를 활용하여 합계 계산을 더 간결하게 만들었습니다.

## 깊이 있게 탐구하기

리팩토링은 스몰토크 프로그래밍 커뮤니티에서 그 뿌리를 찾을 수 있으며, 1990년대 마틴 파울러의 책 "Refactoring: Improving the Design of Existing Code"로 널리 알려졌습니다. 이후 수년에 걸쳐 애자일 방법론과 좋은 코딩 관행의 중요한 부분이 되었습니다.

리팩토링에는 테스트 주도 개발(TDD)에서의 Red-Green-Refactor 같은 다양한 접근 방법이 있습니다. 이는 리팩토링이 버그를 도입하지 않도록 실패하는 테스트로 시작하여 테스트를 통과시키고, 그 다음 코드를 정리하는 과정을 보장합니다.

리팩토링을 구현할 때, 프로세스 중에 기능이 손상되지 않도록 하는 포괄적인 테스트 스위트를 가지는 것이 중요합니다. C#에 대한 ReSharper와 같은 자동 리팩토링 도구는 안전한 방법으로 코드 구조를 변경하는 데 도움을 줄 수 있습니다. 그러나 도구는 코드베이스와 코딩 원칙에 대한 깊은 이해를 보완하는 것이어야 합니다.

## 참고자료

- 마틴 파울러의 대표적인 작업인 리팩토링: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Visual Studio에서의 리팩토링에 대한 마이크로소프트 가이드: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- 예시를 통한 리팩토링 패턴에 대한 자세한 살펴보기: [SourceMaking 리팩토링](https://sourcemaking.com/refactoring)
