---
title:                "Java: 테스트 작성"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

코드 테스트를 작성하는 이유는 소프트웨어 개발에서 무언가를 작성할 때 그것이 잘 작동하는지 확인하기 위해서입니다. 테스트는 소스 코드의 신뢰성을 높이고 버그를 방지하는 데에 매우 중요합니다.

## 방법

테스트를 작성하는 것은 다음과 같은 단계를 따릅니다:

1. **테스트 되어야 할 코드 코드의 단위나 기능을 정의하기:** 이 단계에서 우리는 테스트를 작성할 대상을 결정합니다.

2. **사용할 테스트 프레임워크 선택하기:** 자바에서는 다양한 테스트 프레임워크가 있지만, JUnit이 가장 널리 사용되고 있습니다.

3. **테스트 클래스 만들기:** 이제 우리는 테스트 클래스를 만들고, 필요한 의존성을 임포트합니다.

4. **테스트 메서드 작성하기:** 테스트 대상의 각 기능마다 테스트 메서드를 작성합니다. 이 과정에서 예상되는 출력값과 실제 출력값을 비교하여 테스트를 수행합니다.

5. **개별 테스트 실행하기:** 마지막으로, 작성한 테스트 클래스를 실행하여 각 기능이 잘 작동하는지 확인합니다. 만약 오류가 발생하면, 코드를 수정하고 다시 테스트를 실행합니다.

아래는 자바로 만든 간단한 계산기의 테스트 예제입니다:

```Java
import org.junit.Test;
import static org.junit.Assert.*;

public class CalculatorTest {

    Calculator calc = new Calculator();

    @Test
    public void testAdd() {
        assertEquals(4, calc.add(2, 2));
    }

    @Test
    public void testSubtract() {
        assertEquals(2, calc.subtract(4, 2));
    }

    @Test
    public void testMultiply() {
        assertEquals(8, calc.multiply(4, 2));
    }

    @Test
    public void testDivide() {
        assertEquals(2, calc.divide(4, 2));
    }
}
```

위 예제에서는 JUnit 프레임워크를 사용하여 Calculator 클래스의 각 기능에 대한 테스트를 생성하고 실행하는 방법을 보여줍니다.

## 깊이 파보기

코드 테스트를 작성할 때, 주의해야 할 몇 가지 중요한 요소가 있습니다. 첫째, 각 테스트는 독립적으로 실행되어야 합니다. 즉, 하나의 테스트 실패가 다른 테스트에 영향을 미쳐서는 안 됩니다. 둘째, 모든 코드를 테스트하는 것은 불가능하기 때문에, 우리는 중요한 부분을 테스트하는 데 집중해야 합니다. 세번째, 테스트는 자주 실행되고 업데이트되어야 하며, 개발 과정에서 코드와 함께 유지되어야 합니다.

자바에서는 코드 테스트를 작성하기 위해 JUnit 이외에도 TestNG, Mockito와 같은 다른 프레임워크를 사용할 수 있습니다. 깊이 파보기를 통해 다른 프레임워크를 알아보고, 적절한 프레임워크를 선택하여 사용하는 것이 중요합니다.

## 관련 자료

- JUnit: https://junit.org/junit4/
- TestNG: https://testng.org/doc/
- Mockito: https://site.mockito.org/