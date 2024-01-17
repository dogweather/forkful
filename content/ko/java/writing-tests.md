---
title:                "테스트 작성하기"
html_title:           "Java: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
테스트를 작성하는 것은 프로그래머가 자신이 작성한 코드가 예상대로 작동하는지 확인하기 위해 하는 것입니다. 이는 버그를 찾고 수정하고 코드의 품질을 향상시키는 데 도움이 됩니다.

## 방법:
```Java
public class Calculator {

   public static int add(int x, int y) {
         return x + y;
    }

}
```
```Java
public class CalculatorTest {

   public void testAdd() {
         int result = Calculator.add(2, 3);
         assertEqual(result, 5);
    }

}
```

## 깊이 파고들기:
초기에는 테스트를 작성하는 것은 많은 시간과 비용이 들어갔지만, 현재는 자동화된 테스트 도구가 있어 더 쉽고 빠르게 테스트를 작성할 수 있게 되었습니다. 또 다른 대안으로는 테스트 주도 개발(Test Driven Development)이 있습니다. 이는 테스트를 먼저 작성하고 그에 맞게 코드를 작성하는 방식으로, 코드의 품질을 높이고 유지보수 비용을 줄일 수 있습니다. 테스트를 작성할 때에는 코드의 각 기능이 예상한 대로 작동하는지를 확인하는 것 외에도 경계 조건과 예외 상황을 고려해야 합니다.

## 참고자료:
- [메서드 당 하나의 단위 테스트를 유지하라](https://medium.com/@seunghokim_97534/tdd-method-단위-테스트-4d9f814b736)
- [JUnit 테스트 프레임워크](https://junit.org/junit5/)