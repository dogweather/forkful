---
title:                "Kotlin: 테스트 작성하기"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

테스트를 작성하면 왜 좋을까요? 효율적이고 견고한 코드를 작성하는 데 도움이 됩니다.

## 방법

테스트 작성은 쉽습니다! 우리는 Kotlin과 함께 자동차의 엔진 소리를 측정할 수 있는 응용 프로그램을 만들 예정입니다. 다음과 같이 작성하세요:

```Kotlin
fun main() {
    val carSoundCalculator = CarSoundCalculator()
    println(carSoundCalculator.calculateEngineSound())
}

class CarSoundCalculator {
    fun calculateEngineSound(): String {
        return "Vroom Vroom!"
    }
}
```

출력:
```
Vroom Vroom!
```

## 딥 다이브

테스트 코드를 작성함으로써 우리는 코드를 더욱 견고하고 안정적으로 만드는 것뿐만 아니라 변경 사항이 코드에 영향을 미치는지도 확인할 수 있습니다. 더 나아가, 테스트 코드를 작성함으로써 코드를 더 쉽게 이해하고 유지 보수할 수 있습니다.

예를 들어, 위의 자동차 엔진 소리 측정 응용 프로그램에서 우리는 `calculateEngineSound()` 함수의 출력에 대한 테스트를 작성할 수 있습니다. 이는 코드의 변경에 영향을 받는지 여부를 확인하는 데 도움이 될 것입니다.

```
@Test
fun `test calculateEngineSound`() {
    val carSoundCalculator = CarSoundCalculator()
    assertEquals("Vroom Vroom!", carSoundCalculator.calculateEngineSound())
}
```

위의 코드는 `calculateEngineSound()` 함수가 "Vroom Vroom!"을 반환하는지를 테스트하는 내용입니다.

## 더 읽기

- [Kotlin 공식 홈페이지](https://kotlinlang.org/)
- [코틀린으로 테스트 작성하기](https://medium.com/xebia-engineering/writing-tests-with-kotlin-27487edf22d7)
- [Kotlin과 JUnit을 이용한 테스트 코드 작성하기](https://www.baeldung.com/kotlin-testing)
- [소프트웨어 개발에서 자동화 테스트의 중요성](https://www.3pillarglobal.com/insights/software-testing-automation-importance)