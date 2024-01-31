---
title:                "테스트 작성하기"
date:                  2024-01-19
simple_title:         "테스트 작성하기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
테스트 작성은 코드의 기능을 확인하기 위해 사용되는 코드 조각입니다. 프로그래머들은 예측 가능한 결과와 안정성을 보장하기 위해 테스트를 작성합니다.

## How to: (어떻게 하나요?)
Kotlin에서는 JUnit 라이브러리를 사용하여 테스트를 작성할 수 있습니다. 간단한 함수와 이에 대한 테스트 코드 예제를 보여드리겠습니다.

```Kotlin
// 함수 선언
fun add(a: Int, b: Int): Int {
    return a + b
}

// 테스트 클래스
class ExampleUnitTest {
    @Test
    fun addition_isCorrect() {
        assertEquals(4, add(2, 2))
    }
}
```

실행 결과:
```
Test passed
```

## Deep Dive (깊이 탐구하기)
테스트 코드 작성은 20세기 후반 소프트웨어 공학에서 시작되었습니다. 대안으로는 TDD(Test-Driven Development)가 있으며, 이는 테스트를 먼저 작성한 후 실제 코드를 구현하는 방식입니다. Kotlin에서는 JUnit 외에도 MockK, Spek 등 여러 테스트 프레임워크를 사용할 수 있습니다.

## See Also (관련 자료)
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)
