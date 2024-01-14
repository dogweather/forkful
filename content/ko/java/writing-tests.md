---
title:                "Java: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-tests.md"
---

{{< edit_this_page >}}

# 왜 테스트를 작성해야 할까요?

테스트는 코드의 안정성과 신뢰성을 검증하는 데에 필수적입니다. 테스트를 작성하면 코드에서 발생할 수 있는 버그들을 미리 발견하여 실제 작동 시 문제가 되지 않도록 예방할 수 있습니다.

## 어떻게 작성하나요?

```Java
// 예시 코드
public class Calculator{
    public int add(int a, int b){
        return a + b;
    }
}

// 테스트 코드
@Test
public void testAdd(){
    Calculator calculator = new Calculator();
    int result = calculator.add(3,5);
    assertEquals(8, result);
}
```

위의 예시 코드와 테스트 코드를 보면, `Calculator` 클래스의 `add()` 메소드를 테스트하는 코드를 작성하고 있습니다. `add()` 메소드가 제대로 작동하는지를 확인하기 위해 `assertEquals()` 메소드를 사용하여 예상 값과 실제 값을 비교하고 있습니다.

이처럼 간단한 예시 코드를 기반으로 테스트 코드를 작성하면 됩니다. 테스트 코드를 작성하는 것이 귀찮거나 시간이 많이 걸릴 수도 있지만, 이렇게 작성한 테스트 코드는 나중에 버그를 찾거나 코드를 수정할 때 큰 도움이 될 것입니다.

## 더 깊게 알아보기

테스트 코드를 작성할 때, `assert` 메소드들을 이해하고 사용하는 것이 중요합니다. `assertEquals()` 외에도 `assertNotEquals()`, `assertTrue()`, `assertFalse()` 등 다양한 메소드가 있으며, 어떤 상황에 어떤 메소드를 사용해야 하는지를 잘 파악해야 합니다.

또한, 테스트를 작성할 때는 모든 가능한 경우를 고려해야 합니다. 예를 들어, 위의 예시에서는 양의 정수를 더하는 경우만을 테스트하고 있기 때문에 음의 정수나 0을 더하는 경우에 대한 테스트도 추가로 작성하는 것이 좋습니다.

## 관련 자료

- [JUnit5 공식 문서](https://junit.org/junit5/docs/current/user-guide/#writing-tests-assertions)
- [마크다운(Markdown) 정리](https://gist.github.com/ihoneymon/652be052a0727ad59601)
- [Java 예외 처리하기](https://brunch.co.kr/@alden/1)