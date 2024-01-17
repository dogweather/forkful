---
title:                "테스트 작성하기"
html_title:           "C#: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

테스트 작성이란 무엇인지, 그리고 프로그래머들이 이를 왜 하는지에 대해 간단히 설명하겠습니다. 테스트 작성은 코드를 작동하기 전에 실제로 코드를 시험해보는 것을 말합니다. 이로써 버그를 찾고 수정하며 더 품질 좋은 코드를 만들 수 있습니다.

## 하는 법:

아래에 코드 블록 안에 코딩 예제와 샘플 출력을 함께 제시하겠습니다.

```C#
public class Calculator 
{
  public int Add(int num1, int num2) 
  {
    return num1 + num2;
  }
}
```
```C#
// Calculator 클래스의 인스턴스 생성
Calculator calc = new Calculator();

// Add 메서드 사용
int result = calc.Add(5, 7);
Console.WriteLine(result); // 출력: 12
```

## 깊이 들어가기:

테스트 작성에 대한 깊은 정보를 다루겠습니다. 테스트 작성은 오랜 역사를 가지고 있으며, 이를 위해 다양한 방법이 제시되어 왔습니다. 하지만 현재는 자동화된 테스트 프레임워크를 사용하여 코드의 품질을 검증하는 것이 일반적입니다.

## 참고 자료:

- [테스트 작성에 대한 참고 자료](https://en.wikipedia.org/wiki/Test-driven_development)
- [C#과 테스트 작성](https://docs.microsoft.com/en-us/dotnet/core/testing)
- [테스트 작성에 대한 더 깊은 이해](https://medium.com/@everis_tech/things-you-should-know-about-test-driven-development-tdd-53a2fcca4266)