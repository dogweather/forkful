---
title:    "Java: 테스트 작성하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-tests.md"
---

{{< edit_this_page >}}

# 왜?

프로그래머로서 테스트 작성을 왜 해야 할까요? 테스트는 우리의 코드를 검증하고 버그를 찾는 데 매우 중요합니다. 테스트 작성이 생산성과 코드의 신뢰성을 높이는 데 도움을 줄 수 있으며 최종 제품의 품질을 보장하는 데도 큰 역할을 합니다.

## 어떻게 해야 할까요?

자바 프로그래밍에서 테스트 작성하는 방법을 알아보겠습니다. 다음은 자바 메소드를 테스트하는 간단한 예제입니다. 

```Java
public class Calculator {

    public static int add(int a, int b) {
        return a + b;
    }

    public static int subtract(int a, int b) {
        return a - b;
    }
}

public class CalculatorTest {

    @Test
    public void testAdd() {
        int result = Calculator.add(5, 10);
        assertEquals(15, result);
    }

    @Test
    public void testSubtract() {
        int result = Calculator.subtract(10, 5);
        assertEquals(5, result);
    }
}
```

위의 예제는 `Calculator` 클래스의 `add()`와 `subtract()` 메소드를 각각 테스트하는 방법을 보여줍니다. `@Test` 어노테이션을 이용하여 각 테스트를 정의하고, `assertEquals()` 메소드를 사용하여 예상 결과와 실제 결과가 일치하는지를 확인합니다.

## 깊이 파고들기

테스트 작성에 대해 더 깊이 알아봅시다. 테스트에는 몇 가지 유형이 있습니다. 예를 들어, 단위 테스트는 각각의 기능이 올바르게 작동하는지를 검증하는 것이고, 통합 테스트는 각 모듈이 서로 잘 동작하는지를 확인하는 것입니다.

또한 테스트를 작성할 때 주의해야 할 몇 가지 중요한 점이 있습니다. 모든 코드를 테스트해야 하는 것은 아니며, 주요 로직과 예외 사항을 테스트하는 것이 중요합니다. 또한 테스트가 오래 걸리거나 매우 복잡한 경우에는 테스트를 줄이거나 분리하는 것이 좋습니다.

## 또 다른 참고자료

- [JUnit 공식 홈페이지](https://junit.org/junit5/)
- [Java 테스트 작성 가이드](https://www.javacodegeeks.com/2018/06/java-testing-guide-pros-cons-smart-tips-to-get-started.html)
- [TDD(테스트 주도 개발)란?](https://www.slipp.net/questions/59)