---
title:    "Java: 테스트 작성하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 왜 테스트를 작성하는가

테스트 작성은 개발 과정에서 필수적입니다. 테스트를 작성함으로써 코드의 품질을 향상시키고 버그를 줄일 수 있습니다.

## 작성 방법

테스트를 작성하는 가장 기본적인 방법은 JUnit을 사용하는 것입니다. JUnit은 자바 단위 테스트 프레임워크로, 간단한 메서드부터 복잡한 클래스까지 다양한 상황에서 테스트를 작성할 수 있습니다. 아래는 JUnit을 이용하여 간단한 메서드의 테스트를 작성하는 예제입니다.

```Java
public class Calculator {

    public int add(int num1, int num2) {
        return num1 + num2;
    }

    public int subtract(int num1, int num2) {
        return num1 - num2;
    }
}
```

위 클래스의 메서드를 테스트하는 JUnit 코드는 다음과 같습니다.

```Java
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calc = new Calculator();
        int result = calc.add(5, 10);
        assertEquals(15, result);
    }
    
    @Test
    public void testSubtract() {
        Calculator calc = new Calculator();
        int result = calc.subtract(10, 5);
        assertEquals(5, result);
    }
}
```

테스트는 메서드의 반환값과 예상한 값을 비교하여 일치하지 않으면 실패로 표시됩니다. 이렇게 테스트를 작성하면 코드를 변경할 때마다 테스트를 수행하여 버그를 발견하고 수정할 수 있습니다.

## 깊게 들어가기

테스트 작성에는 여러 가지 기술과 패턴이 존재합니다. 예를 들어, TDD(Test-Driven Development)는 테스트를 작성하고 그 테스트를 통과할 수 있는 코드를 작성하는 개발 방법론입니다. 또한, Mockito와 같은 모의 프레임워크를 사용하여 외부 의존성을 모방하거나, 스프링 프레임워크에서 제공하는 테스트 관련 어노테이션을 사용하여 테스트를 조금 더 간결하게 작성할 수도 있습니다. 더 자세한 내용은 관련 링크를 참고해주세요.

## 관련 링크

- [JUnit](https://junit.org/junit4/)
- [Mockito](https://site.mockito.org/)
- [TDD란 무엇인가?](https://en.wikipedia.org/wiki/Test-driven_development)
- [스프링 테스트 관련 어노테이션](https://docs.spring.io/spring/docs/current/spring-framework-reference/testing.html#integration-testing-annotations)