---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
테스트 작성은 코드가 의도대로 잘 작동하는지 확인하는 과정입니다. 버그를 초기에 발견하고, 나중에 코드를 수정할 때 문제 없이 기능이 유지되게 하기 위해 프로그래머들은 이를 수행합니다.

## How to: (어떻게 하나요?)
Java에서 JUnit을 사용하여 간단한 테스트를 만들어 봅시다. 아래 예제 코드는 `Calculator` 클래스의 `add` 메소드를 테스트합니다.

```java
import org.junit.jupiter.api.*;

class Calculator {
    int add(int a, int b) {
        return a + b;
    }
}

public class CalculatorTests {
    private Calculator calculator = new Calculator();

    @Test
    void testAdd() {
        Assertions.assertEquals(5, calculator.add(2, 3));
    }
}

```

테스트를 실행하면 다음과 같은 결과가 나타납니다:

```
Test passed.
```

## Deep Dive (심층 분석)
테스트 작성은 TDD(Test-Driven Development)의 핵심이며, 1990년대 중반 Kent Beck에 의해 널리 퍼졌습니다. 대안으로 BDD(Behavior-Driven Development)가 있습니다. JUnit 라이브러리 주요 구성 요소는 `Assertions` 클래스, `Test` 어노테이션, 그리고 테스트 실행기입니다.

## See Also (추가 정보)
- JUnit 5 User Guide: https://junit.org/junit5/docs/current/user-guide/
- Test-Driven Development by Example by Kent Beck: ISBN-13: 978-0321146533
- Introduction to Mockito (for mocking in tests): https://site.mockito.org