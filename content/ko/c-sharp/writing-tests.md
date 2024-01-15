---
title:                "테스트 작성하기."
html_title:           "C#: 테스트 작성하기."
simple_title:         "테스트 작성하기."
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

테스트의 중요성은 소프트웨어의 안정성과 신뢰성을 보장하기 위해서입니다. 테스트를 통해 버그를 발견하고 수정할 수 있으며, 더 나은 코드를 작성할 수 있습니다.

## 어떻게

첫번째 예제에서는 간단한 연산을 테스트하는 코드를 보여 드리겠습니다.

```C#
using System;

class Program
{
    static void Main()
    {
        // 덧셈 계산을 테스트합니다.
        int result = Add(2, 3);

        // 올바른 결과값인지 확인합니다.
        if (result == 5)
        {
            Console.WriteLine("테스트가 성공적으로 통과했습니다.");
        }
        else
        {
            Console.WriteLine("테스트가 실패했습니다.");
        }
    }

    // 덧셈 함수
    public int Add(int num1, int num2)
    {
        return num1 + num2;
    }
}
```

출력 결과는 다음과 같습니다.

```
테스트가 성공적으로 통과했습니다.
```

두번째 예제에서는 배열을 정렬하는 함수를 테스트해보겠습니다.

```C#
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        // 정렬할 배열
        int[] nums = { 5, 2, 7, 1 };

        // 배열을 정렬합니다.
        Array.Sort(nums);

        // 올바른 결과값인지 확인합니다.
        if (nums.SequenceEqual(new[] { 1, 2, 5, 7 }))
        {
            Console.WriteLine("테스트가 성공적으로 통과했습니다.");
        }
        else
        {
            Console.WriteLine("테스트가 실패했습니다.");
        }
    }
}
```

출력 결과는 다음과 같습니다.

```
테스트가 성공적으로 통과했습니다.
```

## 딥 다이브

테스트를 작성하기 전에 반드시 테스트의 목적을 명확하게 정의해야 합니다. 또한 여러 가지 테스트 도구를 활용하여 코드가 잘 동작하는지 확인하고 불필요한 버그를 사전에 방지할 수 있습니다.

## 참고자료

- [Microsoft 공식 C# 문서](https://docs.microsoft.com/ko-kr/dotnet/csharp/)
- [C# 테스트 작성 가이드](https://docs.microsoft.com/ko-kr/dotnet/core/testing/)
- [xUnit 닷넷 프로젝트](https://xunit.net/)